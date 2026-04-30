{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

-- Position tracking tests, extracted to a separate module so that
-- mhs (whose 'toplevel' compile pass overflows on very large source
-- modules) can compile each module independently.
module PosTests (posTests) where

import MiniParser.Base
import MiniParser.Parser
import TestHelpers (getPosFromResult)
import Test.HUnit
import Data.Char (isDigit)

posTests :: [Test]
posTests =
  -- =====================================================================
  -- Position tracking tests
  -- initPos is Pos 1 1, advancePos increments col, newline resets to next line col 1
  -- =====================================================================
  -- Single character: starts at Pos 1 1, after consuming 'a' -> Pos 1 2
  [ "pos: single char" ~:
    getPosFromResult (parse item "abc") @?= Just (Pos 1 2)
  -- Three characters on one line
  , "pos: three chars" ~:
    getPosFromResult (parse (do _ <- item; _ <- item; item) "abc") @?= Just (Pos 1 4)
  -- String match advances by length
  , "pos: string" ~:
    getPosFromResult (parse (string "hello") "helloworld") @?= Just (Pos 1 6)
  -- Newline advances to next line
  , "pos: newline" ~:
    getPosFromResult (parse (do _ <- item; item) "a\nb") @?= Just (Pos 2 1)
  -- Multiple newlines: \n -> Pos 2 1, \n -> Pos 3 1, 'c' -> Pos 3 2
  , "pos: multiple newlines" ~:
    getPosFromResult (parse (do _ <- item; _ <- item; item) "\n\nc") @?= Just (Pos 3 2)
  -- Mixed content across lines: 'a'->col 2, 'b'->col 3, \n->Pos 2 1, 'c'->col 2, 'd'->col 3
  , "pos: mixed lines" ~:
    getPosFromResult (parse (string "ab\ncd") "ab\ncdef") @?= Just (Pos 2 3)
  -- identHaskell position: "foo" is 3 chars on line 1
  , "pos: identHaskell" ~:
    getPosFromResult (parse identHaskell "foo rest") @?= Just (Pos 1 4)
  -- dec position: "123" is 3 chars
  , "pos: dec" ~:
    getPosFromResult (parse dec "123rest") @?= Just (Pos 1 4)
  -- signed decimal "-42" consumes 3 chars -> col 4
  , "pos: signed decimal negative" ~:
    getPosFromResult (parse (signed decimal :: Parser Int) "-42rest") @?= Just (Pos 1 4)
  -- signed decimal "+42" also consumes 3 chars
  , "pos: signed decimal explicit positive" ~:
    getPosFromResult (parse (signed decimal :: Parser Int) "+42rest") @?= Just (Pos 1 4)
  -- signed decimal "42" consumes 2 chars
  , "pos: signed decimal unsigned" ~:
    getPosFromResult (parse (signed decimal :: Parser Int) "42rest") @?= Just (Pos 1 3)
  -- Position tests don't compare values, so the polymorphic Num a parsers
  -- need an explicit annotation to avoid defaulting warnings.
  -- hex base-level: "ff" is 2 chars -> col 3
  , "pos: hex" ~:
    getPosFromResult (parse (hex :: Parser Int) "ffrest") @?= Just (Pos 1 3)
  -- oct base-level: "777" is 3 chars
  , "pos: oct" ~:
    getPosFromResult (parse (oct :: Parser Int) "777rest") @?= Just (Pos 1 4)
  -- bin base-level: "1010" is 4 chars
  , "pos: bin" ~:
    getPosFromResult (parse (bin :: Parser Int) "1010rest") @?= Just (Pos 1 5)
  -- hexidecimal: "0xff" is 4 chars -> col 5
  , "pos: hexidecimal" ~:
    getPosFromResult (parse (hexidecimal :: Parser Int) "0xff rest") @?= Just (Pos 1 5)
  -- octal: "0o777" is 5 chars -> col 6
  , "pos: octal" ~:
    getPosFromResult (parse (octal :: Parser Int) "0o777rest") @?= Just (Pos 1 6)
  -- binary: "0b1010" is 6 chars -> col 7
  , "pos: binary" ~:
    getPosFromResult (parse (binary :: Parser Int) "0b1010rest") @?= Just (Pos 1 7)
  -- hexidecimal with leading whitespace: "  0x10" consumes 6 chars -> col 7
  , "pos: hexidecimal with leading spaces" ~:
    getPosFromResult (parse (hexidecimal :: Parser Int) "  0x10") @?= Just (Pos 1 7)
  -- lookAhead does NOT advance position
  , "pos: lookAhead no advance" ~:
    getPosFromResult (parse lookAhead "hello") @?= Just (Pos 1 1)
  -- lookAheadMulti does NOT advance position
  , "pos: lookAheadMulti no advance" ~:
    getPosFromResult (parse (lookAheadMulti 3) "hello") @?= Just (Pos 1 1)
  -- pTakeWhile advances by consumed chars
  , "pos: pTakeWhile" ~:
    getPosFromResult (parse (pTakeWhile isDigit) "123abc") @?= Just (Pos 1 4)
  -- pTakeWhile no match: position unchanged
  , "pos: pTakeWhile no match" ~:
    getPosFromResult (parse (pTakeWhile isDigit) "abc") @?= Just (Pos 1 1)
  -- takeUntil advances by chars before delimiter
  , "pos: takeUntil" ~:
    getPosFromResult (parse (takeUntil '#') "foo#bar") @?= Just (Pos 1 4)
  -- takeUntil' advances past delimiter too
  , "pos: takeUntil'" ~:
    getPosFromResult (parse (takeUntil' '#') "foo#bar") @?= Just (Pos 1 5)
  -- takeUntilStr advances by chars before needle
  , "pos: takeUntilStr" ~:
    getPosFromResult (parse (takeUntilStr "end") "startendfinish") @?= Just (Pos 1 6)
  -- takeUntilStr' advances past needle
  , "pos: takeUntilStr'" ~:
    getPosFromResult (parse (takeUntilStr' "end") "startendfinish") @?= Just (Pos 1 9)
  -- space advances through whitespace
  , "pos: space" ~:
    getPosFromResult (parse space "   abc") @?= Just (Pos 1 4)
  -- space with newline: ' '->col 2, ' '->col 3, \n->Pos 2 1, ' '->col 2, ' '->col 3
  , "pos: space with newline" ~:
    getPosFromResult (parse space "  \n  abc") @?= Just (Pos 2 3)
  -- eof at empty input: position unchanged
  , "pos: eof" ~:
    getPosFromResult (parse eof "") @?= Just (Pos 1 1)
  -- takeAll consumes everything: 'a'->col 2, 'b'->col 3, \n->Pos 2 1, 'c'->col 2, 'd'->col 3
  , "pos: takeAll" ~:
    getPosFromResult (parse takeAll "ab\ncd") @?= Just (Pos 2 3)
  -- takeUntilEOL stops before newline
  , "pos: takeUntilEOL" ~:
    getPosFromResult (parse takeUntilEOL "hello\nworld") @?= Just (Pos 1 6)
  -- takeUntilEOL' consumes past newline
  , "pos: takeUntilEOL'" ~:
    getPosFromResult (parse takeUntilEOL' "hello\nworld") @?= Just (Pos 2 1)
  -- Sequential parsers accumulate position: "ab"->Pos 1 3, \n->Pos 2 1, "cd"->Pos 2 3
  , "pos: sequential across newline" ~:
    getPosFromResult (parse (do _ <- string "ab"; _ <- item; string "cd") "ab\ncdef") @?= Just (Pos 2 3)
  -- Multi-line string
  , "pos: multi-line string parse" ~:
    getPosFromResult (parse (do _ <- takeUntilEOL'; _ <- takeUntilEOL'; takeUntilEOL')
      "line1\nline2\nline3\n") @?= Just (Pos 4 1)
  ]
