{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import MiniParser.Base
import MiniParser.Parser (signed, fp, float, expFloat)
import TestHelpers
  ( stripPos, runHUnit, runQC, suiteHeader, sectionHeader, summaryLine )
import Test.HUnit
import Test.QuickCheck (Property, forAll, choose)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import System.Exit (exitFailure)

runP :: Parser a -> Text -> Either [Error] (a, Text)
runP p = stripPos . parse p

closeEnough :: Double -> Double -> Bool
closeEnough a b
  | isNaN a && isNaN b           = True
  | isInfinite a && isInfinite b = signum a == signum b
  | b == 0                       = abs a < 1e-300
  | otherwise                    = abs (a - b) / abs b < 1e-12

-- Assert the parser produces (value, remainder), with value compared via closeEnough.
parseEq :: Parser Double -> Text -> Double -> Text -> Assertion
parseEq p inp expV expR =
  case runP p inp of
    Right (v, r)
      | closeEnough v expV && r == expR -> pure ()
    other -> assertFailure $
      "input "    ++ show inp ++
      "\nexpected: Right (" ++ show expV ++ ", " ++ show expR ++ ")" ++
      "\ngot:      " ++ show other

-- Assert the parser fails on this input.
parseFail :: Parser Double -> Text -> Assertion
parseFail p inp =
  case runP p inp of
    Left _   -> pure ()
    Right ok -> assertFailure $
      "input " ++ show inp ++ ": expected failure, got " ++ show ok

-- ---------------------------------------------------------------------------
-- HUnit tests
-- ---------------------------------------------------------------------------

rawTests :: Test
rawTests = TestLabel "fp (raw, cap = 9, strict-fractional)" $ TestList
  -- Strict-fractional rejects bare integer-shape input. The input must
  -- have a '.' digits or an 'e'/'E' digits component (or both). This
  -- matches Megaparsec's Text.Megaparsec.Char.Lexer.float semantics.
  [ "bare 0 rejected (no frac, no exp)"           ~: parseFail (fp 9) "0"
  , "bare 42 rejected (no frac, no exp)"          ~: parseFail (fp 9) "42"
  , "leading zeros bare rejected"                 ~: parseFail (fp 9) "007"
  -- decimals (have '.' digits, accepted)
  , "3.14"          ~: parseEq (fp 9) "3.14"        3.14    ""
  , "0.5"           ~: parseEq (fp 9) "0.5"         0.5     ""
  , "00.50"         ~: parseEq (fp 9) "00.50"       0.5     ""
  , "many fractional digits"
                    ~: parseEq (fp 9) "0.123456789" 0.123456789 ""
  -- exponents (have 'e' digits, accepted even without '.')
  , "3e5"           ~: parseEq (fp 9) "3e5"         3e5     ""
  , "3E5"           ~: parseEq (fp 9) "3E5"         3e5     ""
  , "3e+5"          ~: parseEq (fp 9) "3e+5"        3e5     ""
  , "3e-5"          ~: parseEq (fp 9) "3e-5"        3e-5    ""
  , "3.14e2"        ~: parseEq (fp 9) "3.14e2"      314.0   ""
  , "3.14e-2"       ~: parseEq (fp 9) "3.14e-2"     0.0314  ""
  , "0e0"           ~: parseEq (fp 9) "0e0"         0.0     ""
  -- remainders: trailing junk after a valid frac is lenient (consumed
  -- only up through the valid number).
  , "trailing letters"      ~: parseEq (fp 9) "3.14abc"   3.14 "abc"
  , "trailing space"        ~: parseEq (fp 9) "3.14 r"    3.14 " r"
  , "trailing dot+digits"   ~: parseEq (fp 9) "3.5.7"     3.5  ".7"
  -- These previously parsed as bare-integer 3 with the partial component
  -- in the remainder. Under strict semantics, the resulting "no frac, no
  -- exp" shape is rejected as a whole (the parser does not commit to a
  -- bare integer because that would be lenient).
  , "double dot rejected (3 with no valid frac)"
                            ~: parseFail (fp 9) "3..5"
  , "trailing dot only rejected (3 with no valid frac)"
                            ~: parseFail (fp 9) "3."
  , "e but no digits rejected (3 with no valid exp)"
                            ~: parseFail (fp 9) "3e"
  , "e then sign no digits rejected"
                            ~: parseFail (fp 9) "3e+"
  , "e then non-digit rejected"
                            ~: parseFail (fp 9) "3eX"
  -- failures (unchanged from prior strict-or-lenient surface)
  , "empty"            ~: parseFail (fp 9) ""
  , "letters only"     ~: parseFail (fp 9) "abc"
  , "leading dot"      ~: parseFail (fp 9) ".5"
  , "leading minus"    ~: parseFail (fp 9) "-3"
  , "leading plus"     ~: parseFail (fp 9) "+3"
  , "leading e"        ~: parseFail (fp 9) "e5"
  , "leading whitespace" ~: parseFail (fp 9) "  3.14"
  -- numeric extremes (Double)
  , "1e308"            ~: parseEq (fp 9) "1e308"      1e308  ""
  , "1e-300"           ~: parseEq (fp 9) "1e-300"     1e-300 ""
  ]

capTests :: Test
capTests = TestLabel "exponent cap (DoS guard)" $ TestList
  -- These tests use small exponent VALUES (zero-padded to the cap length) so
  -- readFloat doesn't allocate huge intermediate Integers. The cap counts
  -- input *digits*, not the parsed value, so leading zeros count.
  [ "cap 9: 9-digit accepted"      ~: parseEq   (fp 9) "1e000000005"  1e5  ""
  , "cap 9: 9-digit neg accepted"  ~: parseEq   (fp 9) "1e-000000005" 1e-5 ""
  , "cap 9: 10-digit rejected"     ~: parseFail (fp 9) "1e1000000000"
  , "cap 9: 10-digit neg rejected" ~: parseFail (fp 9) "1e-1000000000"
  , "cap 3: 3-digit accepted"      ~: parseEq   (fp 3) "1e005"        1e5  ""
  , "cap 3: 4-digit rejected"      ~: parseFail (fp 3) "1e1234"
  , "cap 0: 1-digit rejected"      ~: parseFail (fp 0) "1.5e1"
  , "cap 0: no-exp accepted"       ~: parseEq   (fp 0) "3.14"         3.14 ""
  -- The original DoS vector: 12 bytes, 10-digit exponent. Used to peg CPU.
  , "DoS '1e1000000000' rejected"  ~: parseFail (fp 9) "1e1000000000"
  -- 1000-nine exponent: tiny input, exponent ~10^999. Used to take 8+ seconds.
  , "DoS '1e<1000 9s>' rejected"
      ~: parseFail (fp 9) (T.pack ("1e" ++ replicate 1000 '9'))
  ]

floatTests :: Test
floatTests = TestLabel "float (token-aware, default cap = 4, strict-fractional)" $ TestList
  [ "leading whitespace" ~: parseEq float "  3.14"        3.14  ""
  , "leading newline"    ~: parseEq float "\n\t3.14"      3.14  ""
  , "line comment"       ~: parseEq float "-- nope\n3.14" 3.14  ""
  , "block comment"      ~: parseEq float "{- gone -}3.14" 3.14 ""
  -- Strict-fractional: bare integer rejected. Use 'scientific' for lenient.
  , "bare integer rejected" ~: parseFail float "42"
  , "exponent-only accepted" ~: parseEq float "42e0" 42.0 ""
  , "fractional accepted"    ~: parseEq float "42.0" 42.0 ""
  , "4-digit exp accepted (Infinity)" ~: parseEq float "1e9999" (1/0) ""
  , "5-digit exp rejected" ~: parseFail float "1e10000"
  , "10-digit exp rejected" ~: parseFail float "1e1000000000"
  , "empty rejected"     ~: parseFail float ""
  ]

expFloatTests :: Test
expFloatTests = TestLabel "expFloat (caller-supplied cap)" $ TestList
  [ "expFloat 6 accepts 6-digit exponent"
      ~: parseEq   (expFloat 6) "1e000005"  1e5 ""
  , "expFloat 6 rejects 7-digit exponent"
      ~: parseFail (expFloat 6) "1e9999999"
  , "expFloat 0 rejects any exponent"
      ~: parseFail (expFloat 0) "3.14e1"
  , "expFloat 0 accepts no exponent"
      ~: parseEq   (expFloat 0) "3.14"       3.14  ""
  , "expFloat strips leading whitespace/comments"
      ~: parseEq   (expFloat 4) "  -- c\n3.14" 3.14 ""
  ]

signedFloatTests :: Test
signedFloatTests = TestLabel "signed float" $ TestList
  [ "signed float -3.14"
      ~: parseEq (signed float) "-3.14"        (-3.14) ""
  , "signed float +3.14"
      ~: parseEq (signed float) "+3.14"          3.14  ""
  , "signed float bare"
      ~: parseEq (signed float) "3.14"           3.14  ""
  , "signed float leading whitespace before sign"
      ~: parseEq (signed float) "  -3.14"      (-3.14) ""
  , "signed float mid-sign whitespace rejected"
      ~: parseFail (signed float) "- 3.14"
  , "signed float over cap rejects whole input"
      ~: parseFail (signed float) "-1e10000"
  ]

-- ---------------------------------------------------------------------------
-- QuickCheck: round-trip via signed float
-- ---------------------------------------------------------------------------

-- For any Double in a sane magnitude range, show then parse back equals input.
-- Bounded to (-1e30, 1e30) to (a) avoid NaN/Infinity, (b) stay well inside the
-- 4-digit exponent cap that signed float inherits from float.
prop_roundTrip :: Property
prop_roundTrip = forAll (choose (-1e30, 1e30 :: Double)) $ \d ->
  let s = T.pack (show d)
  in case parse (signed float :: Parser Double) s of
       Right (v, _, _) -> closeEnough v d
       Left _          -> False

main :: IO ()
main = do
  start <- getCurrentTime
  suiteHeader "Float tests"
  (hp1, hf1) <- runHUnit rawTests
  (hp2, hf2) <- runHUnit capTests
  (hp3, hf3) <- runHUnit floatTests
  (hp4, hf4) <- runHUnit expFloatTests
  (hp5, hf5) <- runHUnit signedFloatTests
  sectionHeader "QuickCheck"
  qcOK <- runQC "round-trip via signed float" prop_roundTrip
  let passed = hp1 + hp2 + hp3 + hp4 + hp5 + (if qcOK then 1 else 0)
      failed = hf1 + hf2 + hf3 + hf4 + hf5 + (if qcOK then 0 else 1)
  end <- getCurrentTime
  summaryLine passed failed (diffUTCTime end start)
  if failed > 0 then exitFailure else pure ()
