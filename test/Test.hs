{-# LANGUAGE OverloadedStrings #-}
-- The numeric parsers (dec/hex/oct/bin and friends) are polymorphic over
-- Num. In this test module we rely on GHC's default-Integer for
-- comparisons like `Right (42, " foo")` — that's intentional, not a bug,
-- so we silence -Wtype-defaults here rather than annotating every test.
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Main where

import MiniParser.Base
import MiniParser.Parser
import TestHelpers (stripPos, runHUnit, runQC,
                    suiteHeader, sectionHeader, summaryLine)
import PosTests (posTests)
import Test.HUnit
import Test.QuickCheck
import Data.Char (isDigit, isAlpha, isAlphaNum, isLower, isUpper, isHexDigit, isOctDigit, intToDigit)
import Control.Applicative (Alternative(..), many)
import Data.Text (Text)
import qualified Data.Text as T
import Numeric (showHex, showOct, showIntAtBase)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import System.Exit (exitFailure)

-- HUnit tests for specific parser behavior, positive and negative
hunitTests :: Test
hunitTests = TestList $ concat
  [ -- EXISTING TESTS (from original Test.hs)
    [ "parse item success" ~: stripPos (parse item "abc") ~?= Right ('a', "bc")
  , "parse item failure (empty)" ~: parse item "" ~?= Left [EndOfInput]
  , "parse digit success" ~: stripPos (parse digit "5xyz") ~?= Right ('5', "xyz")
  , "parse digit failure (non-digit)" ~: parse digit "abc" ~?= Left [Unexpected' "a"]
  , "parse digit failure (empty)" ~: parse digit "" ~?= Left [EndOfInput]
  , "parse letter success" ~: stripPos (parse letter "abc") ~?= Right ('a', "bc")
  , "parse letter failure (digit)" ~: parse letter "123" ~?= Left [Unexpected' "1"]
  , "parse letter failure (empty)" ~: parse letter "" ~?= Left [EndOfInput]
  , "parse identifierHaskell success" ~: stripPos (parse identifierHaskell "foo123 ") ~?= Right ("foo123", " ")
  , "parse identifierHaskell failure (capital start)" ~: parse identifierHaskell "Foo123" ~?= Left [Unexpected' "F"]
  , "parse decimal success" ~: stripPos (parse decimal "42 foo") ~?= Right (42, " foo")
  , "parse decimal failure (non-digit start)" ~: parse decimal "abc" ~?= Left [Unexpected' "a"]
  , "parse decimal failure (empty)" ~: parse decimal "" ~?= Left [EndOfInput]
  , "parse string success" ~: stripPos (parse (string "foo") "foobar") ~?= Right ("foo", "bar")
  , "parse string failure" ~: parse (string "foo") "bar" ~?= Left [Unexpected "foo" "bar"]
  , "parse char success" ~: stripPos (parse (char 'x') "xyz") ~?= Right ('x', "yz")
  , "parse char failure" ~: parse (char 'x') "abc" ~?= Left [Unexpected' "a"]
  , "parse lookAhead success" ~: stripPos (parse lookAhead "hello") ~?= Right ('h', "hello")
  , "parse lookAhead failure" ~: parse lookAhead "" ~?= Left [EndOfInput]
  , "parse row success" ~: stripPos (parse row "test row\nnext") ~?= Right ("test row", "next")
  , "parse row failure (empty)" ~:
    case parse row ("" :: Text) of
      Left errs -> assertBool "Expected Empty in errors" (Empty `elem` errs)
      _ -> assertFailure "Expected Left [Error]"
  , "parse row failure (spaces only)" ~: parse row " \n" ~?= Left [Empty]
  , "parse until '#' success" ~: stripPos (parse (takeUntil '#') "foo#bar") ~?= Right ("foo", "#bar")
  , "parse until '#' no match" ~: stripPos (parse (takeUntil '#') "foobarbaz") ~?= Right ("foobarbaz", "")
  , "parse until and consume '#' success" ~: stripPos (parse (takeUntil' '#') "foo#bar") ~?= Right ("foo", "bar")
  , "parse until and consume '#' no match" ~: parse (takeUntil' '#') "foobar" ~?= Left [EndOfInput]
  , "parse eof success" ~: stripPos (parse eof "") ~?= Right ((), "")
  , "parse eof failure" ~: parse eof "data" ~?= Left [ExpectedEndOfFile 'd']
  , "parse <|> fallback" ~: stripPos (parse (char 'x' <|> char 'y') "yup") ~?= Right ('y', "up")
  , "parse many letters" ~: stripPos (parse (many letter) "abc123") ~?= Right ("abc", "123")
  , "parse many digits empty" ~: stripPos (parse (many digit) "abc") ~?= Right ("", "abc")
  -- Basic character parsers
  , "parse alphanum success (letter)" ~: stripPos (parse alphanum "a123") ~?= Right ('a', "123")
  , "parse alphanum success (digit)" ~: stripPos (parse alphanum "5abc") ~?= Right ('5', "abc")
  , "parse alphanum failure" ~: parse alphanum "!@#" ~?= Left [Unexpected' "!"]
  , "parse alphanum failure (empty)" ~: parse alphanum "" ~?= Left [EndOfInput]

  , "parse lower success" ~: stripPos (parse lower "abc") ~?= Right ('a', "bc")
  , "parse lower failure (uppercase)" ~: parse lower "ABC" ~?= Left [Unexpected' "A"]
  , "parse lower failure (digit)" ~: parse lower "123" ~?= Left [Unexpected' "1"]
  , "parse lower failure (empty)" ~: parse lower "" ~?= Left [EndOfInput]
  , "parse upper success" ~: stripPos (parse upper "ABC") ~?= Right ('A', "BC")
  , "parse upper failure (lowercase)" ~: parse upper "abc" ~?= Left [Unexpected' "a"]
  , "parse upper failure (digit)" ~: parse upper "123" ~?= Left [Unexpected' "1"]
  , "parse upper failure (empty)" ~: parse upper "" ~?= Left [EndOfInput]
  -- Tokenized character parser
  , "parse character success" ~: stripPos (parse (character 'f') "  f  bar") ~?= Right ('f', "  bar")
  , "parse character failure" ~: parse (character 'f') "  g  bar" ~?= Left [Unexpected' "g"]
  -- Basic parser combinators
  , "parse identHaskell success" ~: stripPos (parse identHaskell "foo123") ~?= Right ("foo123", "")
  , "parse identHaskell failure (uppercase start)" ~: parse identHaskell "Foo123" ~?= Left [Unexpected' "F"]
  , "parse identHaskell failure (digit start)" ~: parse identHaskell "123foo" ~?= Left [Unexpected' "1"]
  , "parse dec success" ~: stripPos (parse dec "123") ~?= Right (123, "")
  , "parse dec success with remainder" ~: stripPos (parse dec "456abc") ~?= Right (456, "abc")
  , "parse dec failure (no digits)" ~: parse dec "abc" ~?= Left [Unexpected' "a"]
    ]
  , -- Base-level hex (no prefix)
    [ "parse hex success (single digit)" ~: stripPos (parse hex "0") ~?= Right (0, "")
  , "parse hex success (lower f)" ~: stripPos (parse hex "f") ~?= Right (15, "")
  , "parse hex success (upper F)" ~: stripPos (parse hex "F") ~?= Right (15, "")
  , "parse hex success (ff)" ~: stripPos (parse hex "ff") ~?= Right (255, "")
  , "parse hex success (mixed case)" ~: stripPos (parse hex "aBcDeF") ~?= Right (0xABCDEF, "")
  , "parse hex success (1A)" ~: stripPos (parse hex "1A") ~?= Right (26, "")
  , "parse hex success with remainder" ~: stripPos (parse hex "ffxyz") ~?= Right (255, "xyz")
  , "parse hex stops at non-hex digit 'g'" ~: stripPos (parse hex "ffg") ~?= Right (255, "g")
  , "parse hex failure (non-hex start)" ~: parse hex "g" ~?= Left [Unexpected' "g"]
  , "parse hex failure (empty)" ~: parse hex "" ~?= Left [EndOfInput]
  , "parse hex does NOT strip whitespace" ~: parse hex "  ff" ~?= Left [Unexpected' " "]
  -- Base-level oct (no prefix)
  , "parse oct success (0)" ~: stripPos (parse oct "0") ~?= Right (0, "")
  , "parse oct success (7)" ~: stripPos (parse oct "7") ~?= Right (7, "")
  , "parse oct success (777)" ~: stripPos (parse oct "777") ~?= Right (511, "")
  , "parse oct success (10)" ~: stripPos (parse oct "10") ~?= Right (8, "")
  , "parse oct stops at '8'" ~: stripPos (parse oct "778") ~?= Right (63, "8")
  , "parse oct stops at '9'" ~: stripPos (parse oct "79") ~?= Right (7, "9")
  , "parse oct failure ('8')" ~: parse oct "8" ~?= Left [Unexpected' "8"]
  , "parse oct failure ('9')" ~: parse oct "9" ~?= Left [Unexpected' "9"]
  , "parse oct failure (letter)" ~: parse oct "a" ~?= Left [Unexpected' "a"]
  , "parse oct failure (empty)" ~: parse oct "" ~?= Left [EndOfInput]
  -- Base-level bin (no prefix)
  , "parse bin success (0)" ~: stripPos (parse bin "0") ~?= Right (0, "")
  , "parse bin success (1)" ~: stripPos (parse bin "1") ~?= Right (1, "")
  , "parse bin success (1010)" ~: stripPos (parse bin "1010") ~?= Right (10, "")
  , "parse bin success (8 ones = 255)" ~: stripPos (parse bin "11111111") ~?= Right (255, "")
  , "parse bin stops at '2'" ~: stripPos (parse bin "102") ~?= Right (2, "2")
  , "parse bin failure ('2')" ~: parse bin "2" ~?= Left [Unexpected' "2"]
  , "parse bin failure ('9')" ~: parse bin "9" ~?= Left [Unexpected' "9"]
  , "parse bin failure (empty)" ~: parse bin "" ~?= Left [EndOfInput]
  -- Base-level digs (generic): sanity-check pmult dispatch via hex/oct/bin above
  , "parse digs isDigit 10 = dec behavior" ~:
    stripPos (parse (digs isDigit 10) "42abc") ~?= Right (42, "abc")
  -- hexidecimal (Parser level, with "0x" prefix)
  , "parse hexidecimal success (0x0)" ~: stripPos (parse hexidecimal "0x0") ~?= Right (0, "")
  , "parse hexidecimal success (0x1)" ~: stripPos (parse hexidecimal "0x1") ~?= Right (1, "")
  , "parse hexidecimal success (0xff)" ~: stripPos (parse hexidecimal "0xff") ~?= Right (255, "")
  , "parse hexidecimal success (0xFF)" ~: stripPos (parse hexidecimal "0xFF") ~?= Right (255, "")
  , "parse hexidecimal success (0Xff capital X)" ~: stripPos (parse hexidecimal "0Xff") ~?= Right (255, "")
  , "parse hexidecimal success (0x1A mixed case)" ~: stripPos (parse hexidecimal "0x1A") ~?= Right (26, "")
  , "parse hexidecimal success (0xABCDEF)" ~:
    stripPos (parse hexidecimal "0xABCDEF") ~?= Right (11259375, "")
  , "parse hexidecimal strips leading whitespace" ~:
    stripPos (parse hexidecimal "  0x10") ~?= Right (16, "")
  , "parse hexidecimal strips Haskell line comment" ~:
    stripPos (parse hexidecimal "-- ignored\n0xff") ~?= Right (255, "")
  , "parse hexidecimal strips Haskell block comment" ~:
    stripPos (parse hexidecimal "{- ignored -}0xff") ~?= Right (255, "")
  , "parse hexidecimal with trailing remainder" ~:
    stripPos (parse hexidecimal "0x10 rest") ~?= Right (16, " rest")
  , "parse hexidecimal stops at non-hex digit" ~:
    stripPos (parse hexidecimal "0x1Ag") ~?= Right (26, "g")
  , "parse hexidecimal failure (empty)" ~: parse hexidecimal "" ~?= Left [EndOfInput]
  , "parse hexidecimal failure (no prefix, decimal digit)" ~:
    parse hexidecimal "42" ~?= Left [Unexpected' "4"]
  , "parse hexidecimal failure (wrong first char)" ~:
    parse hexidecimal "x10" ~?= Left [Unexpected' "x"]
  , "parse hexidecimal failure (bare 0, nothing after)" ~:
    parse hexidecimal "0" ~?= Left [EndOfInput]
  , "parse hexidecimal failure (wrong second char)" ~:
    parse hexidecimal "0y10" ~?= Left [Unexpected' "y"]
  , "parse hexidecimal failure (prefix then EOF)" ~:
    parse hexidecimal "0x" ~?= Left [EndOfInput]
  , "parse hexidecimal failure (prefix then non-hex)" ~:
    parse hexidecimal "0xg" ~?= Left [Unexpected' "g"]
  , "parse hexidecimal failure (prefix then space)" ~:
    parse hexidecimal "0x ff" ~?= Left [Unexpected' " "]
  -- octal (Parser level, with "0o" prefix)
  , "parse octal success (0o0)" ~: stripPos (parse octal "0o0") ~?= Right (0, "")
  , "parse octal success (0o7)" ~: stripPos (parse octal "0o7") ~?= Right (7, "")
  , "parse octal success (0o17)" ~: stripPos (parse octal "0o17") ~?= Right (15, "")
  , "parse octal success (0O17 capital O)" ~: stripPos (parse octal "0O17") ~?= Right (15, "")
  , "parse octal success (0o777)" ~: stripPos (parse octal "0o777") ~?= Right (511, "")
  , "parse octal success (0o10)" ~: stripPos (parse octal "0o10") ~?= Right (8, "")
  , "parse octal strips leading whitespace" ~:
    stripPos (parse octal "  0o17") ~?= Right (15, "")
  , "parse octal stops at '8' (not an octal digit)" ~:
    stripPos (parse octal "0o778") ~?= Right (63, "8")
  , "parse octal failure (empty)" ~: parse octal "" ~?= Left [EndOfInput]
  , "parse octal failure (no prefix)" ~: parse octal "42" ~?= Left [Unexpected' "4"]
  , "parse octal failure (prefix then EOF)" ~: parse octal "0o" ~?= Left [EndOfInput]
  , "parse octal failure (prefix then '8')" ~: parse octal "0o8" ~?= Left [Unexpected' "8"]
  , "parse octal failure (prefix then '9')" ~: parse octal "0o9" ~?= Left [Unexpected' "9"]
  , "parse octal failure (wrong prefix letter)" ~: parse octal "0p1" ~?= Left [Unexpected' "p"]
  -- binary (Parser level, with "0b" prefix)
  , "parse binary success (0b0)" ~: stripPos (parse binary "0b0") ~?= Right (0, "")
  , "parse binary success (0b1)" ~: stripPos (parse binary "0b1") ~?= Right (1, "")
  , "parse binary success (0b10)" ~: stripPos (parse binary "0b10") ~?= Right (2, "")
  , "parse binary success (0b1010)" ~: stripPos (parse binary "0b1010") ~?= Right (10, "")
  , "parse binary success (0B1010 capital B)" ~:
    stripPos (parse binary "0B1010") ~?= Right (10, "")
  , "parse binary success (0b11111111)" ~:
    stripPos (parse binary "0b11111111") ~?= Right (255, "")
  , "parse binary strips leading whitespace" ~:
    stripPos (parse binary "  0b101") ~?= Right (5, "")
  , "parse binary stops at '2'" ~:
    stripPos (parse binary "0b102") ~?= Right (2, "2")
  , "parse binary failure (empty)" ~: parse binary "" ~?= Left [EndOfInput]
  , "parse binary failure (no prefix)" ~: parse binary "10" ~?= Left [Unexpected' "1"]
  , "parse binary failure (prefix then EOF)" ~: parse binary "0b" ~?= Left [EndOfInput]
  , "parse binary failure (prefix then '2')" ~: parse binary "0b2" ~?= Left [Unexpected' "2"]
  , "parse binary failure (wrong prefix letter)" ~: parse binary "0c1" ~?= Left [Unexpected' "c"]
    ]
  , -- Backtracking with <|>: if the first branch consumes the '0' and fails,
    -- the second branch must still see the original input.
    [ "hexidecimal <|> decimal picks decimal for '42'" ~:
    stripPos (parse (hexidecimal <|> decimal) "42") ~?= Right (42, "")
  , "hexidecimal <|> decimal picks hex for '0x1A'" ~:
    stripPos (parse (hexidecimal <|> decimal) "0x1A") ~?= Right (26, "")
  , "hexidecimal <|> decimal backtracks over consumed '0' for '0123'" ~:
    stripPos (parse (hexidecimal <|> decimal) "0123") ~?= Right (123, "")
  , "prefix cascade picks binary for '0b10'" ~:
    stripPos (parse (binary <|> octal <|> hexidecimal <|> decimal) "0b10") ~?= Right (2, "")
  , "prefix cascade picks octal for '0o10'" ~:
    stripPos (parse (binary <|> octal <|> hexidecimal <|> decimal) "0o10") ~?= Right (8, "")
  , "prefix cascade picks hex for '0x10'" ~:
    stripPos (parse (binary <|> octal <|> hexidecimal <|> decimal) "0x10") ~?= Right (16, "")
  , "prefix cascade falls through to decimal for '10'" ~:
    stripPos (parse (binary <|> octal <|> hexidecimal <|> decimal) "10") ~?= Right (10, "")
  , "prefix cascade falls through to decimal for '010'" ~:
    stripPos (parse (binary <|> octal <|> hexidecimal <|> decimal) "010") ~?= Right (10, "")
  -- signed combinator with decimal
  , "signed decimal: unsigned positive" ~:
    stripPos (parse (signed decimal) "42") ~?= Right (42, "")
  , "signed decimal: explicit negative" ~:
    stripPos (parse (signed decimal) "-42") ~?= Right (-42, "")
  , "signed decimal: explicit positive" ~:
    stripPos (parse (signed decimal) "+42") ~?= Right (42, "")
  -- 'signed' strips leading whitespace/comments BEFORE the sign, and
  -- requires the digits to follow the sign with no intervening space.
  , "signed decimal: leading whitespace, unsigned" ~:
    stripPos (parse (signed decimal) "  42") ~?= Right (42, "")
  , "signed decimal: leading whitespace, negative" ~:
    stripPos (parse (signed decimal) "  -42") ~?= Right (-42, "")
  , "signed decimal: leading whitespace, explicit positive" ~:
    stripPos (parse (signed decimal) "  +42") ~?= Right (42, "")
  , "signed decimal: leading line comment then negative" ~:
    stripPos (parse (signed decimal) "-- skip\n-42") ~?= Right (-42, "")
  , "signed decimal: leading block comment then positive" ~:
    stripPos (parse (signed decimal) "{- skip -}+42") ~?= Right (42, "")
  -- Strict: no whitespace permitted between the sign and the digits.
  , "signed decimal failure: space between negative sign and digits" ~:
    case parse (signed decimal) ("- 42" :: Text) of
      Left errs -> assertBool "Should have errors" (not $ null errs)
      Right _ -> assertFailure "Should have failed"
  , "signed decimal failure: space between positive sign and digits" ~:
    case parse (signed decimal) ("+ 42" :: Text) of
      Left errs -> assertBool "Should have errors" (not $ null errs)
      Right _ -> assertFailure "Should have failed"
  , "signed decimal failure: leading WS then sign then space" ~:
    case parse (signed decimal) ("  - 42" :: Text) of
      Left errs -> assertBool "Should have errors" (not $ null errs)
      Right _ -> assertFailure "Should have failed"
  -- 'signed' applied to the raw 'dec' parser behaves the same way: leading
  -- whitespace is OK (signed strips it), but space between sign and digits
  -- is rejected (the lookahead inside signed catches it).
  , "signed dec: leading whitespace, negative" ~:
    stripPos (parse (signed dec) "  -42") ~?= Right (-42, "")
  , "signed dec failure: space between negative sign and digits (raw)" ~:
    case parse (signed dec) ("- 42" :: Text) of
      Left errs -> assertBool "Should have errors" (not $ null errs)
      Right _ -> assertFailure "Should have failed"
  , "signed dec failure: space between positive sign and digits (raw)" ~:
    case parse (signed dec) ("+ 42" :: Text) of
      Left errs -> assertBool "Should have errors" (not $ null errs)
      Right _ -> assertFailure "Should have failed"
  , "signed decimal: trailing remainder preserved" ~:
    stripPos (parse (signed decimal) "-42 rest") ~?= Right (-42, " rest")
  , "signed decimal: zero" ~:
    stripPos (parse (signed decimal) "0") ~?= Right (0, "")
  , "signed decimal: -0 round-trips to 0" ~:
    stripPos (parse (signed decimal) "-0") ~?= Right (0, "")
  -- signed combinator failure cases
  , "signed decimal failure (empty)" ~:
    parse (signed decimal) "" ~?= Left [EndOfInput]
  , "signed decimal failure (sign with no digits)" ~:
    case parse (signed decimal) ("-" :: Text) of
      Left errs -> assertBool "Should have errors" (not $ null errs)
      Right _ -> assertFailure "Should have failed"
  , "signed decimal failure (non-digit)" ~:
    case parse (signed decimal) ("abc" :: Text) of
      Left errs -> assertBool "Should have errors" (not $ null errs)
      Right _ -> assertFailure "Should have failed"
  , "signed decimal failure (sign then non-digit)" ~:
    case parse (signed decimal) ("-abc" :: Text) of
      Left errs -> assertBool "Should have errors" (not $ null errs)
      Right _ -> assertFailure "Should have failed"
  , "signed decimal failure (double negative)" ~:
    case parse (signed decimal) ("--42" :: Text) of
      Left errs -> assertBool "Should have errors" (not $ null errs)
      Right _ -> assertFailure "Should have failed"
  , "signed decimal failure (mixed signs)" ~:
    case parse (signed decimal) ("+-42" :: Text) of
      Left errs -> assertBool "Should have errors" (not $ null errs)
      Right _ -> assertFailure "Should have failed"
    ]
  , -- signed is polymorphic; works with the other base parsers too.
    -- For each base, exercise: bare positive, '-' negative, '+' explicit
    -- positive, and at least one larger value matching real-world inputs.
    [ "signed hexidecimal: positive" ~:
    stripPos (parse (signed hexidecimal) "0xff") ~?= Right (255, "")
  , "signed hexidecimal: negative" ~:
    stripPos (parse (signed hexidecimal) "-0xff") ~?= Right (-255, "")
  , "signed hexidecimal: explicit positive" ~:
    stripPos (parse (signed hexidecimal) "+0x10") ~?= Right (16, "")
  , "signed hexidecimal: -0xfed1" ~:
    stripPos (parse (signed hexidecimal) "-0xfed1") ~?= Right (-0xfed1, "")
  , "signed hexidecimal: +0xFED1 (explicit, mixed case)" ~:
    stripPos (parse (signed hexidecimal) "+0xFED1") ~?= Right (0xfed1, "")
  , "signed octal: positive (no sign)" ~:
    stripPos (parse (signed octal) "0o17") ~?= Right (15, "")
  , "signed octal: negative" ~:
    stripPos (parse (signed octal) "-0o17") ~?= Right (-15, "")
  , "signed octal: explicit positive" ~:
    stripPos (parse (signed octal) "+0o17") ~?= Right (15, "")
  , "signed octal: +0o7541647" ~:
    stripPos (parse (signed octal) "+0o7541647") ~?= Right (2016167, "")
  , "signed octal: -0o7541647" ~:
    stripPos (parse (signed octal) "-0o7541647") ~?= Right (-2016167, "")
  , "signed binary: positive without prefix sign" ~:
    stripPos (parse (signed binary) "0b101") ~?= Right (5, "")
  , "signed binary: negative" ~:
    stripPos (parse (signed binary) "-0b1010") ~?= Right (-10, "")
  , "signed binary: explicit positive" ~:
    stripPos (parse (signed binary) "+0b1010") ~?= Right (10, "")
  , "signed binary: -0b10001010001" ~:
    stripPos (parse (signed binary) "-0b10001010001") ~?= Right (-1105, "")
  , "signed binary: +0b10001010001" ~:
    stripPos (parse (signed binary) "+0b10001010001") ~?= Right (1105, "")
  -- Leading whitespace before the sign is allowed for every base.
  , "signed hexidecimal: leading WS, negative '  -0xfed1'" ~:
    stripPos (parse (signed hexidecimal) "  -0xfed1") ~?= Right (-0xfed1, "")
  , "signed hexidecimal: leading WS, explicit positive '  +0xff'" ~:
    stripPos (parse (signed hexidecimal) "  +0xff") ~?= Right (255, "")
  , "signed octal: leading WS, negative '  -0o17'" ~:
    stripPos (parse (signed octal) "  -0o17") ~?= Right (-15, "")
  , "signed octal: leading WS, explicit positive '  +0o17'" ~:
    stripPos (parse (signed octal) "  +0o17") ~?= Right (15, "")
  , "signed binary: leading WS, negative '  -0b1010'" ~:
    stripPos (parse (signed binary) "  -0b1010") ~?= Right (-10, "")
  , "signed binary: leading WS, explicit positive '  +0b1010'" ~:
    stripPos (parse (signed binary) "  +0b1010") ~?= Right (10, "")
  -- Space between sign and digits is rejected for every base.
  , "signed hexidecimal failure: space between sign and digits '- 0xff'" ~:
    case parse (signed hexidecimal) ("- 0xff" :: Text) of
      Left errs -> assertBool "Should have errors" (not $ null errs)
      Right _ -> assertFailure "Should have failed"
  , "signed hexidecimal failure: leading WS then sign then space '  - 0xff'" ~:
    case parse (signed hexidecimal) ("  - 0xff" :: Text) of
      Left errs -> assertBool "Should have errors" (not $ null errs)
      Right _ -> assertFailure "Should have failed"
  , "signed octal failure: space between sign and digits '- 0o17'" ~:
    case parse (signed octal) ("- 0o17" :: Text) of
      Left errs -> assertBool "Should have errors" (not $ null errs)
      Right _ -> assertFailure "Should have failed"
  , "signed binary failure: space between sign and digits '- 0b1010'" ~:
    case parse (signed binary) ("- 0b1010" :: Text) of
      Left errs -> assertBool "Should have errors" (not $ null errs)
      Right _ -> assertFailure "Should have failed"
  -- signed across a prefix cascade
  , "signed cascade picks negative hex" ~:
    stripPos (parse (signed (binary <|> octal <|> hexidecimal <|> decimal)) "-0x10") ~?= Right (-16, "")
  , "signed cascade picks negative decimal fallback" ~:
    stripPos (parse (signed (binary <|> octal <|> hexidecimal <|> decimal)) "-42") ~?= Right (-42, "")
    ]
  , -- Look ahead parsers
    [ "parse lookAheadMulti success" ~: stripPos (parse (lookAheadMulti 3) "hello") ~?= Right ("hel", "hello")
  , "parse lookAheadMulti exact length" ~: stripPos (parse (lookAheadMulti 5) "hello") ~?= Right ("hello", "hello")
  , "parse lookAheadMulti failure (too short)" ~: parse (lookAheadMulti 10) "hello" ~?= Left [EndOfInput]
  , "parse lookAheadMulti empty input" ~: parse (lookAheadMulti 1) "" ~?= Left [EndOfInput]
  -- pTakeWhile parser
  , "parse pTakeWhile digits" ~: stripPos (parse (pTakeWhile isDigit) "123abc") ~?= Right ("123", "abc")
  , "parse pTakeWhile no match" ~: stripPos (parse (pTakeWhile isDigit) "abc123") ~?= Right ("", "abc123")
  , "parse pTakeWhile empty input" ~: stripPos (parse (pTakeWhile isDigit) "") ~?= Right ("", "")
  , "parse pTakeWhile all match" ~: stripPos (parse (pTakeWhile isAlpha) "abc") ~?= Right ("abc", "")
  -- takeAll parser
  , "parse takeAll success" ~: stripPos (parse takeAll "everything here") ~?= Right ("everything here", "")
  , "parse takeAll empty" ~: stripPos (parse takeAll "") ~?= Right ("", "")
  -- String-based parsers
  , "parse takeUntilStr success" ~: stripPos (parse (takeUntilStr "end") "startendfinish") ~?= Right ("start", "endfinish")
  , "parse takeUntilStr no match takes all" ~: stripPos (parse (takeUntilStr "xyz") "abcdef") ~?= Right ("abcdef", "")
  , "parse takeUntilStr at beginning" ~: stripPos (parse (takeUntilStr "start") "startend") ~?= Right ("", "startend")
  , "parse takeUntilStr' success" ~: stripPos (parse (takeUntilStr' "end") "startendfinish") ~?= Right ("start", "finish")
  , "parse takeUntilStr' no match fails on consume" ~: parse (takeUntilStr' "xyz") "abcdef" ~?= Left [Unexpected "xyz" ""]
  -- EOL parsers
  , "parse takeUntilEOL success" ~: stripPos (parse takeUntilEOL "line content\nrest") ~?= Right ("line content", "\nrest")
  , "parse takeUntilEOL no newline" ~: stripPos (parse takeUntilEOL "line content") ~?= Right ("line content", "")
  , "parse takeUntilEOL empty" ~: stripPos (parse takeUntilEOL "") ~?= Right ("", "")
  , "parse takeUntilEOL' success" ~: stripPos (parse takeUntilEOL' "line content\nrest") ~?= Right ("line content", "rest")
  , "parse takeUntilEOL' with \\r\\n" ~: stripPos (parse takeUntilEOL' "line content\r\nrest") ~?= Right ("line content", "rest")
  , "parse takeUntilEOL' no newline" ~: stripPos (parse takeUntilEOL' "line content") ~?= Right ("line content", "")
  -- Delimiter list parser
  , "parse delimList success" ~: stripPos (parse (delimList ',' (string "item")) "item,item,item") ~?= Right (["item", "item", "item"], "")
  , "parse delimList single item" ~: stripPos (parse (delimList ',' (string "item")) "item") ~?= Right (["item"], "")
  , "parse delimList empty" ~: stripPos (parse (delimList ',' (string "item")) "") ~?= Right ([], "")
  , "parse delimList with spaces" ~: stripPos (parse (delimList ',' identifierHaskell) "foo, bar, baz") ~?= Right (["foo", "bar", "baz"], "")
  -- Space and token parsers
  , "parse space success" ~: stripPos (parse space "   \t\n  abc") ~?= Right ((), "abc")
  , "parse space no spaces" ~: stripPos (parse space "abc") ~?= Right ((), "abc")
  , "parse space empty" ~: stripPos (parse space "") ~?= Right ((), "")
  , "parse token success" ~: stripPos (parse (token (string "hello")) "  hello  world") ~?= Right ("hello", "  world")
  , "parse token no spaces" ~: stripPos (parse (token (string "hello")) "helloworld") ~?= Right ("hello", "world")
  , "parse symbol success" ~: stripPos (parse (symbol "==") "  ==  rest") ~?= Right ("==", "  rest")
  -- Choice parser
  , "parse choice success (first)" ~: stripPos (parse (choice [string "foo", string "bar"]) "foobar") ~?= Right ("foo", "bar")
  , "parse choice success (second)" ~: stripPos (parse (choice [string "foo", string "bar"]) "barfoo") ~?= Right ("bar", "foo")
  , "parse choice failure" ~: case parse (choice [string "foo", string "bar"]) ("baz" :: Text) of
      Left errs -> assertBool "Should have errors" (not $ null errs)
      Right _ -> assertFailure "Should have failed"
  , "parse choice empty list" ~: case parse (choice [] :: Parser Text) ("anything" :: Text) of
      Left errs -> assertBool "Should have Empty error" (Empty `elem` errs)
      Right _ -> assertFailure "Should have failed"
  -- Failure parsers
  , "parse pFailStr" ~: case parse (pFailStr "test error" :: Parser String) ("input" :: Text) of
      Left [CustomError msg] -> msg @?= "test error"
      Left errs -> assertFailure $ "Expected CustomError, got: " ++ show errs
      Right _ -> assertFailure "Should have failed"
  , "parse pFail" ~: case parse (pFail [CustomError "error1", CustomError "error2"] :: Parser [Error]) ("input" :: Text) of
      Left errs -> errs @?= [CustomError "error1", CustomError "error2"]
      Right _ -> assertFailure "Should have failed"
  -- pComments (whitespace only via default MiniParser.Comments)
  , "parse pDiscard [] strips whitespace" ~:
    stripPos (parse (pDiscard []) "   \t\n\r\n   rest") ~?= Right ((), "rest")
  , "parse pDiscard [] no space" ~: stripPos (parse (pDiscard []) "rest") ~?= Right ((), "rest")
  , "parse pDiscard [] empty input" ~: stripPos (parse (pDiscard []) ("" :: Text)) ~?= Right ((), "")

  -- Trim parser
  , "parse trim success" ~: stripPos (parse trim "  content with spaces  \n  ") ~?= Right ("content with spaces", "  ")
  , "parse trim only spaces" ~: case parse trim ("   \n  " :: Text) of
      Left errs -> assertBool "Should have Empty error" (Empty `elem` errs)
      Right _ -> assertFailure "Should have failed for empty content"
  , "parse trim content with embedded comment chars" ~:
    stripPos (parse trim "  a = b // not stripped here\n") ~?= Right ("a = b // not stripped here", "")
  -- Split lines functionality
  , "splitLinesT normal" ~: splitLinesT "foo\nbar\nbaz" ~?= ["foo", "bar", "baz"]
  , "splitLinesT mixed endings" ~: splitLinesT "foo\nbar\r\nbaz" ~?= ["foo", "bar", "baz"]
  , "splitLinesT empty" ~: splitLinesT "" ~?= []
  , "splitLinesT single line" ~: splitLinesT "single line" ~?= ["single line"]
  -- Error handling
  , "errorsToString test" ~:
    errorsToString [EndOfInput, CustomError "test"] @?= "EndOfInput CustomError \"test\""
    ]
  -- Position tracking tests live in PosTests (separate module) so that
  -- mhs's toplevel compile pass doesn't overflow on a single huge module.
  , posTests
  ]

-- QuickCheck properties for positive cases
-- These use stripPos to ignore position in comparisons
prop_parseItem_nonEmpty :: Char -> String -> Bool
prop_parseItem_nonEmpty c cs =
  stripPos (parse item (T.pack (c:cs))) == Right (c, T.pack cs)

prop_parseItem_empty :: Bool
prop_parseItem_empty = parse item T.empty == Left [EndOfInput]

prop_parseDigit_success :: Property
prop_parseDigit_success = forAll (elements ['0'..'9']) $ \c ->
  stripPos (parse digit (T.singleton c)) == Right (c, T.empty)

prop_parseDigit_failure :: Property
prop_parseDigit_failure = forAll (suchThat arbitrary (not . isDigit)) $ \c ->
  parse digit (T.singleton c) == Left [Unexpected' [c]]

prop_parseLetter_success :: Property
prop_parseLetter_success = forAll (suchThat arbitrary isAlpha) $ \c ->
  stripPos (parse letter (T.singleton c)) == Right (c, T.empty)

prop_parseLetter_failure :: Property
prop_parseLetter_failure = forAll (suchThat arbitrary (not . isAlpha)) $ \c ->
  parse letter (T.singleton c) == Left [Unexpected' [c]]

prop_parseAlphanum_success :: Property
prop_parseAlphanum_success = forAll (suchThat arbitrary isAlphaNum) $ \c ->
  stripPos (parse alphanum (T.singleton c)) == Right (c, T.empty)

prop_parseAlphanum_failure :: Property
prop_parseAlphanum_failure = forAll (suchThat arbitrary (not . isAlphaNum)) $ \c ->
  parse alphanum (T.singleton c) == Left [Unexpected' [c]]

prop_parseLower_success :: Property
prop_parseLower_success = forAll (elements ['a'..'z']) $ \c ->
  stripPos (parse lower (T.singleton c)) == Right (c, T.empty)

prop_parseLower_failure :: Property
prop_parseLower_failure = forAll (suchThat arbitrary (not . isLower)) $ \c ->
  parse lower (T.singleton c) == Left [Unexpected' [c]]

prop_parseUpper_success :: Property
prop_parseUpper_success = forAll (elements ['A'..'Z']) $ \c ->
  stripPos (parse upper (T.singleton c)) == Right (c, T.empty)

prop_parseUpper_failure :: Property
prop_parseUpper_failure = forAll (suchThat arbitrary (not . isUpper)) $ \c ->
  parse upper (T.singleton c) == Left [Unexpected' [c]]

prop_parseLookAhead_nonEmpty :: Char -> Property
prop_parseLookAhead_nonEmpty c = property $
  let t = T.singleton c in stripPos (parse lookAhead t) == Right (c, t)

prop_parseLookAhead_empty :: Bool
prop_parseLookAhead_empty = parse lookAhead T.empty == Left [EndOfInput]

-- Custom generator for guaranteed presence of sep
genWithSep :: Gen (Char, String)
genWithSep = do
  sep <- arbitrary
  pre <- listOf arbitrary
  post <- listOf arbitrary
  return (sep, pre ++ [sep] ++ post)

prop_parseTakeUntil_success :: Property
prop_parseTakeUntil_success = forAll genWithSep $ \(sep, input) ->
  let tinput = T.pack input
      before = T.takeWhile (/= sep) tinput
      after  = T.dropWhile (/= sep) tinput
  in stripPos (parse (takeUntil sep) tinput) === Right (before, after)

prop_parseTakeUntil_failure :: Property
prop_parseTakeUntil_failure = forAll arbitrary $ \sep ->
  forAll (listOf (suchThat arbitrary (/= sep))) $ \xs ->
    let t = T.pack xs in stripPos (parse (takeUntil sep) t) === Right (t, T.empty)

prop_parseTakeUntil'_success :: Property
prop_parseTakeUntil'_success = forAll genWithSep $ \(sep, input) ->
  let tinput = T.pack input
      before = T.takeWhile (/= sep) tinput
      after  = T.dropWhile (/= sep) tinput
  in not (T.null after) ==> stripPos (parse (takeUntil' sep) tinput) === Right (before, T.tail after)

prop_parseTakeUntil'_failure :: Property
prop_parseTakeUntil'_failure =
  forAll (listOf1 (elements ['a'..'z'])) $ \xs ->
    '#' `notElem` xs ==> parse (takeUntil' '#') (T.pack xs) == Left [EndOfInput]

prop_parseEOF_success :: Bool
prop_parseEOF_success = stripPos (parse eof T.empty) == Right ((), T.empty)

prop_parseEOF_failure :: String -> Property
prop_parseEOF_failure str = not (null str) ==>
  parse eof (T.pack str) == Left [ExpectedEndOfFile (head str)]

prop_parseTakeWhile :: String -> Property
prop_parseTakeWhile input =
  let tinput = T.pack input
      ds     = T.takeWhile isDigit tinput
      rest   = T.dropWhile isDigit tinput
  in stripPos (parse (pTakeWhile isDigit) tinput) === Right (ds, rest)

prop_takeAll :: String -> Property
prop_takeAll input =
  let t = T.pack input in stripPos (parse takeAll t) === Right (t, T.empty)

prop_parseString_success :: String -> String -> Property
prop_parseString_success target input =
  T.pack target `T.isPrefixOf` T.pack input ==>
    let tt = T.pack target
        ti = T.pack input
    in stripPos (parse (string tt) ti) === Right (tt, T.drop (T.length tt) ti)

-- Numeric parsers are polymorphic over Num, so properties need to fix a
-- concrete type at the generator (or parser call) to avoid ambiguity.
prop_parseNat_success :: Property
prop_parseNat_success = forAll (choose (0, 999999 :: Int)) $ \n ->
  let str = T.pack (show n) in stripPos (parse dec str) === Right (n, T.empty)

-- `signed decimal` should round-trip any Int via `show` (which renders
-- negatives with a leading '-' and non-negatives bare).
prop_signedDecimal_success :: Property
prop_signedDecimal_success = forAll (choose (-999999, 999999 :: Int)) $ \n ->
  let str = T.pack (show n)
  in stripPos (parse (signed decimal) str) === Right (n, T.empty)

-- An explicit '+' prefix is also accepted and yields the same value.
prop_signedDecimal_explicitPlus :: Property
prop_signedDecimal_explicitPlus = forAll (choose (0, 999999 :: Int)) $ \n ->
  let str = T.pack ('+' : show n)
  in stripPos (parse (signed decimal) str) === Right (n, T.empty)

-- `signed` composes with other base parsers. Each property generates a
-- random Int, renders it in its base with a "0x"/"0o"/"0b" prefix, and
-- prepends '-' for negatives and '+' for some positives, then round-trips.
prop_signedHex_success :: Property
prop_signedHex_success = forAll (choose (-0xFFFFFFFF, 0xFFFFFFFF :: Int)) $ \n ->
  let body = T.pack ("0x" ++ showHex (abs n) "")
      str  = if n < 0 then T.cons '-' body else body
  in stripPos (parse (signed hexidecimal) str) === Right (n, T.empty)

prop_signedOct_success :: Property
prop_signedOct_success = forAll (choose (-0xFFFFFFFF, 0xFFFFFFFF :: Int)) $ \n ->
  let body = T.pack ("0o" ++ showOct (abs n) "")
      str  = if n < 0 then T.cons '-' body else body
  in stripPos (parse (signed octal) str) === Right (n, T.empty)

prop_signedBin_success :: Property
prop_signedBin_success = forAll (choose (-0xFFFFFFFF, 0xFFFFFFFF :: Int)) $ \n ->
  let body = T.pack ("0b" ++ showIntAtBase 2 intToDigit (abs n) "")
      str  = if n < 0 then T.cons '-' body else body
  in stripPos (parse (signed binary) str) === Right (n, T.empty)

-- Explicit '+' prefix round-trip on each base (only non-negative values).
prop_signedHex_explicitPlus :: Property
prop_signedHex_explicitPlus = forAll (choose (0, 0xFFFFFFFF :: Int)) $ \n ->
  let str = T.pack ("+0x" ++ showHex n "")
  in stripPos (parse (signed hexidecimal) str) === Right (n, T.empty)

prop_signedOct_explicitPlus :: Property
prop_signedOct_explicitPlus = forAll (choose (0, 0xFFFFFFFF :: Int)) $ \n ->
  let str = T.pack ("+0o" ++ showOct n "")
  in stripPos (parse (signed octal) str) === Right (n, T.empty)

prop_signedBin_explicitPlus :: Property
prop_signedBin_explicitPlus = forAll (choose (0, 0xFFFFFFFF :: Int)) $ \n ->
  let str = T.pack ("+0b" ++ showIntAtBase 2 intToDigit n "")
  in stripPos (parse (signed binary) str) === Right (n, T.empty)

-- Base-level round-trips: render N in base B, parse it, get N back.
prop_parseHex_success :: Property
prop_parseHex_success = forAll (choose (0, 0xFFFFFFFF :: Int)) $ \n ->
  let str = T.pack (showHex n "") in stripPos (parse hex str) === Right (n, T.empty)

prop_parseOct_success :: Property
prop_parseOct_success = forAll (choose (0, 0xFFFFFFFF :: Int)) $ \n ->
  let str = T.pack (showOct n "") in stripPos (parse oct str) === Right (n, T.empty)

prop_parseBin_success :: Property
prop_parseBin_success = forAll (choose (0, 0xFFFFFFFF :: Int)) $ \n ->
  let str = T.pack (showIntAtBase 2 intToDigit n "")
  in stripPos (parse bin str) === Right (n, T.empty)

-- Prefixed variants
prop_parseHexidecimal_success :: Property
prop_parseHexidecimal_success = forAll (choose (0, 0xFFFFFFFF :: Int)) $ \n ->
  let str = T.pack ("0x" ++ showHex n "")
  in stripPos (parse hexidecimal str) === Right (n, T.empty)

prop_parseOctal_success :: Property
prop_parseOctal_success = forAll (choose (0, 0xFFFFFFFF :: Int)) $ \n ->
  let str = T.pack ("0o" ++ showOct n "")
  in stripPos (parse octal str) === Right (n, T.empty)

prop_parseBinary_success :: Property
prop_parseBinary_success = forAll (choose (0, 0xFFFFFFFF :: Int)) $ \n ->
  let str = T.pack ("0b" ++ showIntAtBase 2 intToDigit n "")
  in stripPos (parse binary str) === Right (n, T.empty)

-- Integer round-trip: exercises the `Parser Integer` specialization.
prop_parseHexidecimal_integer :: Property
prop_parseHexidecimal_integer = forAll (choose (0, 2^(128 :: Int) :: Integer)) $ \n ->
  let str = T.pack ("0x" ++ showHex n "")
  in stripPos (parse hexidecimal str) === Right (n, T.empty)

-- Failure: first char must be in the alphabet for each base.
prop_parseHex_rejectNonHex :: Property
prop_parseHex_rejectNonHex =
  forAll (suchThat arbitrary (\c -> not (isHexDigit c) && c /= '\NUL')) $ \c ->
    parse (hex :: Parser Int) (T.singleton c) == Left [Unexpected' [c]]

prop_parseOct_reject8or9 :: Property
prop_parseOct_reject8or9 = forAll (elements "89") $ \c ->
  parse (oct :: Parser Int) (T.singleton c) == Left [Unexpected' [c]]

prop_parseOct_rejectNonOct :: Property
prop_parseOct_rejectNonOct =
  forAll (suchThat arbitrary (\c -> not (isOctDigit c) && c /= '\NUL')) $ \c ->
    parse (oct :: Parser Int) (T.singleton c) == Left [Unexpected' [c]]

prop_parseBin_rejectDigits2to9 :: Property
prop_parseBin_rejectDigits2to9 = forAll (elements "23456789") $ \c ->
  parse (bin :: Parser Int) (T.singleton c) == Left [Unexpected' [c]]

-- Trailing garbage: a valid prefix of hex digits followed by a non-hex char
-- should parse the prefix and leave the rest in the stream.
prop_parseHex_remainder :: Property
prop_parseHex_remainder =
  forAll (choose (0, 0xFFFF :: Int)) $ \n ->
  forAll (suchThat arbitrary (\c -> not (isHexDigit c) && c /= '\NUL')) $ \c ->
    let str = T.pack (showHex n "" ++ [c])
    in stripPos (parse hex str) === Right (n, T.singleton c)

-- Test choice with random parsers
prop_choice_success :: Property
prop_choice_success = forAll (elements ["foo", "bar", "baz"]) $ \target ->
  let tt = T.pack target
  in stripPos (parse (choice (map (string . T.pack) ["foo", "bar", "baz"])) (T.pack (target ++ "rest"))) === Right (tt, "rest")

main :: IO ()
main = do
  start <- getCurrentTime

  suiteHeader "MiniParser core tests"
  (hp, hf) <- runHUnit hunitTests

  suiteHeader "QuickCheck properties"

  sectionHeader "Basic parsers"
  qc1 <- mapM (uncurry runQC)
    [ ("prop_parseItem_nonEmpty",       property prop_parseItem_nonEmpty)
    , ("prop_parseItem_empty",          property prop_parseItem_empty)
    ]

  sectionHeader "Character parsers"
  qc2 <- mapM (uncurry runQC)
    [ ("prop_parseDigit_success",       property prop_parseDigit_success)
    , ("prop_parseDigit_failure",       property prop_parseDigit_failure)
    , ("prop_parseLetter_success",      property prop_parseLetter_success)
    , ("prop_parseLetter_failure",      property prop_parseLetter_failure)
    , ("prop_parseAlphanum_success",    property prop_parseAlphanum_success)
    , ("prop_parseAlphanum_failure",    property prop_parseAlphanum_failure)
    , ("prop_parseLower_success",       property prop_parseLower_success)
    , ("prop_parseLower_failure",       property prop_parseLower_failure)
    , ("prop_parseUpper_success",       property prop_parseUpper_success)
    , ("prop_parseUpper_failure",       property prop_parseUpper_failure)
    ]

  sectionHeader "lookAhead"
  qc3 <- mapM (uncurry runQC)
    [ ("prop_parseLookAhead_nonEmpty",  property prop_parseLookAhead_nonEmpty)
    , ("prop_parseLookAhead_empty",     property prop_parseLookAhead_empty)
    ]

  sectionHeader "takeUntil"
  qc4 <- mapM (uncurry runQC)
    [ ("prop_parseTakeUntil_success",   property prop_parseTakeUntil_success)
    , ("prop_parseTakeUntil_failure",   property prop_parseTakeUntil_failure)
    , ("prop_parseTakeUntil'_success",  property prop_parseTakeUntil'_success)
    , ("prop_parseTakeUntil'_failure",  property prop_parseTakeUntil'_failure)
    ]

  sectionHeader "pTakeWhile / takeAll / EOF / string"
  qc5 <- mapM (uncurry runQC)
    [ ("prop_parseTakeWhile",           property prop_parseTakeWhile)
    , ("prop_takeAll",                  property prop_takeAll)
    , ("prop_parseEOF_success",         property prop_parseEOF_success)
    , ("prop_parseEOF_failure",         property prop_parseEOF_failure)
    , ("prop_parseString_success",      property prop_parseString_success)
    ]

  sectionHeader "Number parsing (decimal / signed)"
  qc6 <- mapM (uncurry runQC)
    [ ("prop_parseNat_success",            property prop_parseNat_success)
    , ("prop_signedDecimal_success",       property prop_signedDecimal_success)
    , ("prop_signedDecimal_explicitPlus",  property prop_signedDecimal_explicitPlus)
    , ("prop_signedHex_success",           property prop_signedHex_success)
    , ("prop_signedOct_success",           property prop_signedOct_success)
    , ("prop_signedBin_success",           property prop_signedBin_success)
    , ("prop_signedHex_explicitPlus",      property prop_signedHex_explicitPlus)
    , ("prop_signedOct_explicitPlus",      property prop_signedOct_explicitPlus)
    , ("prop_signedBin_explicitPlus",      property prop_signedBin_explicitPlus)
    ]

  sectionHeader "Number parsing (hex / oct / bin)"
  qc7 <- mapM (uncurry runQC)
    [ ("prop_parseHex_success",            property prop_parseHex_success)
    , ("prop_parseOct_success",            property prop_parseOct_success)
    , ("prop_parseBin_success",            property prop_parseBin_success)
    , ("prop_parseHexidecimal_success",    property prop_parseHexidecimal_success)
    , ("prop_parseOctal_success",          property prop_parseOctal_success)
    , ("prop_parseBinary_success",         property prop_parseBinary_success)
    , ("prop_parseHexidecimal_integer",    property prop_parseHexidecimal_integer)
    , ("prop_parseHex_rejectNonHex",       property prop_parseHex_rejectNonHex)
    , ("prop_parseOct_reject8or9",         property prop_parseOct_reject8or9)
    , ("prop_parseOct_rejectNonOct",       property prop_parseOct_rejectNonOct)
    , ("prop_parseBin_rejectDigits2to9",   property prop_parseBin_rejectDigits2to9)
    , ("prop_parseHex_remainder",          property prop_parseHex_remainder)
    ]

  sectionHeader "choice"
  qc8 <- mapM (uncurry runQC)
    [ ("prop_choice_success",           property prop_choice_success)
    ]

  let allQC = concat [qc1, qc2, qc3, qc4, qc5, qc6, qc7, qc8]
      qcPass = length (filter id allQC)
      qcFail = length (filter not allQC)
      totalPass = hp + qcPass
      totalFail = hf + qcFail

  end <- getCurrentTime
  summaryLine totalPass totalFail (diffUTCTime end start)

  if totalFail > 0 then exitFailure else pure ()
