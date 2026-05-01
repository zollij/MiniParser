{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import MiniParser.Base
import MiniParser.Parser (signed, scientific, expScientific)
import TestHelpers
  ( stripPos, runHUnit, suiteHeader, summaryLine )
import Test.HUnit
import qualified Data.Scientific as Sci
import Data.Scientific (Scientific)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import System.Exit (exitFailure)

runP :: Parser a -> Text -> Either [Error] (a, Text)
runP p = stripPos . parse p

-- Assert: parser produces (expected value, expected remainder).
parseEq :: Parser Scientific -> Text -> Scientific -> Text -> Assertion
parseEq p inp expV expR = case runP p inp of
  Right (v, r)
    | v == expV && r == expR -> pure ()
  other -> assertFailure $
    "input "    ++ show inp ++
    "\nexpected: Right (" ++ show expV ++ ", " ++ show expR ++ ")" ++
    "\ngot:      " ++ show other

parseFail :: Parser Scientific -> Text -> Assertion
parseFail p inp = case runP p inp of
  Left _   -> pure ()
  Right ok -> assertFailure $
    "input " ++ show inp ++ ": expected failure, got " ++ show ok

-- | Assert a Scientific has the exact (coefficient, exponent) representation.
-- Sci.scientific does NOT normalize, but Sci's Eq compares values, not
-- representations. These shape tests use coefficient/base10Exponent so a
-- regression that changes the path's representation gets caught.
parseShape :: Parser Scientific -> Text -> Integer -> Int -> Text -> Assertion
parseShape p inp expCoeff expExp expR = case runP p inp of
  Right (v, r)
    | r == expR
    , Sci.coefficient v == expCoeff
    , Sci.base10Exponent v == expExp -> pure ()
  other -> assertFailure $
    "input " ++ show inp
    ++ "\nexpected: coefficient=" ++ show expCoeff
    ++ ", base10Exponent=" ++ show expExp
    ++ ", remainder=" ++ show expR
    ++ "\ngot:      " ++ show other

-- ---------------------------------------------------------------------------
-- HUnit tests
-- ---------------------------------------------------------------------------

rawTests :: Test
rawTests = TestLabel "sci (raw, cap = 9)" $ TestList
  -- Integer-shape literals: coefficient = N, exponent = 0.
  [ "0"    ~: parseShape (sci 9) "0"   0  0 ""
  , "42"   ~: parseShape (sci 9) "42"  42 0 ""
  , "007"  ~: parseShape (sci 9) "007" 7  0 ""        -- leading zeros: still Integer 7

  -- Fractional literals: coefficient is digits-without-dot, exponent = -fracLen.
  , "3.14" ~: parseShape (sci 9) "3.14"        314         (-2) ""
  , "0.5"  ~: parseShape (sci 9) "0.5"         5           (-1) ""
  , "many fractional digits"
           ~: parseShape (sci 9) "0.123456789" 123456789   (-9) ""

  -- Exponents adjust the exponent component of the Scientific.
  , "3e5"      ~: parseShape (sci 9) "3e5"     3   5    ""
  , "3E5"      ~: parseShape (sci 9) "3E5"     3   5    ""
  , "3e+5"     ~: parseShape (sci 9) "3e+5"    3   5    ""
  , "3e-5"     ~: parseShape (sci 9) "3e-5"    3   (-5) ""
  , "3.14e2"   ~: parseShape (sci 9) "3.14e2"  314 0    ""  -- 314 × 10^(2 - 2 fracLen) = 314
  , "3.14e-2"  ~: parseShape (sci 9) "3.14e-2" 314 (-4) ""  -- 314 × 10^(-2 - 2)
  , "0e0"      ~: parseShape (sci 9) "0e0"     0   0    ""

  -- Lenient remainders (same surface as fp).
  , "trailing letters"      ~: parseShape (sci 9) "3.14abc" 314 (-2) "abc"
  , "trailing space"        ~: parseShape (sci 9) "3.14 r"  314 (-2) " r"
  , "trailing dot+digits"   ~: parseShape (sci 9) "3.5.7"   35  (-1) ".7"
  , "double dot"            ~: parseShape (sci 9) "3..5"    3   0    "..5"
  , "trailing dot only"     ~: parseShape (sci 9) "3."      3   0    "."
  , "e but no digits"       ~: parseShape (sci 9) "3e"      3   0    "e"
  , "e then sign no digits" ~: parseShape (sci 9) "3e+"     3   0    "e+"
  , "e then non-digit"      ~: parseShape (sci 9) "3eX"     3   0    "eX"

  -- Failures.
  , "empty"          ~: parseFail (sci 9) ""
  , "letters only"   ~: parseFail (sci 9) "abc"
  , "leading dot"    ~: parseFail (sci 9) ".5"
  , "leading minus"  ~: parseFail (sci 9) "-3"     -- sci is sign-free; use signed
  , "leading plus"   ~: parseFail (sci 9) "+3"
  , "leading e"      ~: parseFail (sci 9) "e5"
  , "leading whitespace" ~: parseFail (sci 9) "  3.14"

  -- Numeric extremes — Scientific has no Double-range limit. The literal is
  -- preserved exactly; conversion (or lack thereof) is the caller's choice.
  , "1e308"   ~: parseShape (sci 9) "1e308"   1 308    ""
  , "1e-300"  ~: parseShape (sci 9) "1e-300"  1 (-300) ""
  , "1e9999"  ~: parseShape (sci 9) "1e9999"  1 9999   ""  -- representable in Sci, would Inf in Double
  ]

-- The cap discipline mirrors fp's cap tests: HARD failure when exceeded.
capTests :: Test
capTests = TestLabel "exponent cap (DoS guard)" $ TestList
  [ "cap 9: 9-digit accepted"      ~: parseShape (sci 9) "1e000000005" 1 5     ""
  , "cap 9: 9-digit neg accepted"  ~: parseShape (sci 9) "1e-000000005" 1 (-5) ""
  , "cap 9: 10-digit rejected"     ~: parseFail  (sci 9) "1e1000000000"
  , "cap 9: 10-digit neg rejected" ~: parseFail  (sci 9) "1e-1000000000"
  , "cap 3: 3-digit accepted"      ~: parseShape (sci 3) "1e005" 1 5 ""
  , "cap 3: 4-digit rejected"      ~: parseFail  (sci 3) "1e1234"
  , "cap 0: 1-digit rejected"      ~: parseFail  (sci 0) "1.5e1"
  , "cap 0: no-exp accepted"       ~: parseShape (sci 0) "3.14" 314 (-2) ""
  -- The original DoS vector: 12 bytes, 10-digit exponent.
  , "DoS '1e1000000000' rejected"  ~: parseFail (sci 9) "1e1000000000"
  -- 1000-nine exponent: tiny input, exponent ~10^999.
  , "DoS '1e<1000 9s>' rejected"
      ~: parseFail (sci 9) (T.pack ("1e" ++ replicate 1000 '9'))
  ]

scientificTests :: Test
scientificTests = TestLabel "scientific (token-aware, default cap = 4)" $ TestList
  [ "leading whitespace"
      ~: parseShape scientific "  3.14"  314 (-2) ""
  , "leading newline"
      ~: parseShape scientific "\n\t3.14" 314 (-2) ""
  , "line comment"
      ~: parseShape scientific "-- nope\n3.14" 314 (-2) ""
  , "block comment"
      ~: parseShape scientific "{- gone -}3.14" 314 (-2) ""
  , "bare integer"
      ~: parseShape scientific "42" 42 0 ""
  , "4-digit exp accepted"
      ~: parseShape scientific "1e9999" 1 9999 ""
  , "5-digit exp rejected"
      ~: parseFail  scientific "1e10000"
  , "10-digit exp rejected"
      ~: parseFail  scientific "1e1000000000"
  , "empty rejected"
      ~: parseFail  scientific ""
  ]

expScientificTests :: Test
expScientificTests = TestLabel "expScientific (caller-supplied cap)" $ TestList
  [ "expScientific 6 accepts 6-digit exponent"
      ~: parseShape (expScientific 6) "1e000005" 1 5 ""
  , "expScientific 6 rejects 7-digit exponent"
      ~: parseFail  (expScientific 6) "1e9999999"
  , "expScientific 0 rejects any exponent"
      ~: parseFail  (expScientific 0) "3.14e1"
  , "expScientific 0 accepts no exponent"
      ~: parseShape (expScientific 0) "3.14" 314 (-2) ""
  , "expScientific strips leading whitespace/comments"
      ~: parseShape (expScientific 4) "  -- c\n3.14" 314 (-2) ""
  ]

signedScientificTests :: Test
signedScientificTests = TestLabel "signed scientific" $ TestList
  [ "signed scientific -3.14"
      ~: parseEq (signed scientific) "-3.14" (Sci.scientific (-314) (-2)) ""
  , "signed scientific +3.14"
      ~: parseEq (signed scientific) "+3.14" (Sci.scientific 314 (-2))    ""
  , "signed scientific bare"
      ~: parseEq (signed scientific) "3.14"  (Sci.scientific 314 (-2))    ""
  , "signed scientific leading whitespace before sign"
      ~: parseEq (signed scientific) "  -3.14" (Sci.scientific (-314) (-2)) ""
  , "signed scientific mid-sign whitespace rejected"
      ~: parseFail (signed scientific) "- 3.14"
  , "signed scientific over cap rejects whole input"
      ~: parseFail (signed scientific) "-1e10000"
  ]

-- The shape-distinction property: isInteger separates integer-shape literals
-- from fractional literals exactly. This is the property that motivated
-- adding `sci` in the first place.
shapeDistinctionTests :: Test
shapeDistinctionTests = TestLabel "Sci.isInteger distinguishes shapes" $ TestList
  [ "42 is integer"
      ~: TestCase $ case runP (sci 9) "42" of
           Right (v, "") -> assertBool "isInteger 42" (Sci.isInteger v)
           other         -> assertFailure (show other)
  , "3.14 is NOT integer"
      ~: TestCase $ case runP (sci 9) "3.14" of
           Right (v, "") -> assertBool "not (isInteger 3.14)" (not (Sci.isInteger v))
           other         -> assertFailure (show other)
  , "3.0 IS integer (decimal but value-integral)"
      -- Per Sci.isInteger semantics: 3.0 is value-integer (3 == 30/10),
      -- so it returns True. The "shape" distinction is by value equality,
      -- not by source-text shape. Document this explicitly.
      ~: TestCase $ case runP (sci 9) "3.0" of
           Right (v, "") -> assertBool "isInteger 3.0" (Sci.isInteger v)
           other         -> assertFailure (show other)
  , "1e10 is integer (exponent makes coefficient × 10^10 an integer)"
      ~: TestCase $ case runP (sci 9) "1e10" of
           Right (v, "") -> assertBool "isInteger 1e10" (Sci.isInteger v)
           other         -> assertFailure (show other)
  , "1.5e1 is integer (= 15)"
      ~: TestCase $ case runP (sci 9) "1.5e1" of
           Right (v, "") -> assertBool "isInteger 15" (Sci.isInteger v)
           other         -> assertFailure (show other)
  , "1.5e-1 is NOT integer (= 0.15)"
      ~: TestCase $ case runP (sci 9) "1.5e-1" of
           Right (v, "") -> assertBool "not (isInteger 0.15)" (not (Sci.isInteger v))
           other         -> assertFailure (show other)
  ]

main :: IO ()
main = do
  start <- getCurrentTime
  suiteHeader "Scientific tests"
  (hp1, hf1) <- runHUnit rawTests
  (hp2, hf2) <- runHUnit capTests
  (hp3, hf3) <- runHUnit scientificTests
  (hp4, hf4) <- runHUnit expScientificTests
  (hp5, hf5) <- runHUnit signedScientificTests
  (hp6, hf6) <- runHUnit shapeDistinctionTests
  let passed = hp1 + hp2 + hp3 + hp4 + hp5 + hp6
      failed = hf1 + hf2 + hf3 + hf4 + hf5 + hf6
  end <- getCurrentTime
  summaryLine passed failed (diffUTCTime end start)
  if failed > 0 then exitFailure else pure ()
