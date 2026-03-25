{-# LANGUAGE OverloadedStrings #-}

module Main where

import MiniParser.Base
import MiniParser.Parser
import TestHelpers (stripPos, getPosFromResult)
import Test.HUnit
import Test.QuickCheck
import Data.Char (isDigit, isAlpha, isAlphaNum, isLower, isUpper)
import Control.Applicative (Alternative(..), many)
import Data.Text (Text)
import qualified Data.Text as T

-- HUnit tests for specific parser behavior, positive and negative
hunitTests :: Test
hunitTests = TestList
  -- EXISTING TESTS (from original Test.hs)
  [ "parse item success" ~: stripPos (parse item "abc") ~?= Right ('a', "bc")
  , "parse item failure (empty)" ~: parse item "" ~?= Left [EndOfInput]
  , "parse digit success" ~: stripPos (parse digit "5xyz") ~?= Right ('5', "xyz")
  , "parse digit failure (non-digit)" ~: parse digit "abc" ~?= Left [Unexpected' "a"]
  , "parse digit failure (empty)" ~: parse digit "" ~?= Left [EndOfInput]
  , "parse letter success" ~: stripPos (parse letter "abc") ~?= Right ('a', "bc")
  , "parse letter failure (digit)" ~: parse letter "123" ~?= Left [Unexpected' "1"]
  , "parse letter failure (empty)" ~: parse letter "" ~?= Left [EndOfInput]
  , "parse identifier success" ~: stripPos (parse identifier "foo123 ") ~?= Right ("foo123", " ")
  , "parse identifier failure (capital start)" ~: parse identifier "Foo123" ~?= Left [Unexpected' "F"]
  , "parse natural success" ~: stripPos (parse natural "42 foo") ~?= Right (42, " foo")
  , "parse natural failure (non-digit start)" ~: parse natural "abc" ~?= Left [Unexpected' "a"]
  , "parse natural failure (empty)" ~: parse natural "" ~?= Left [EndOfInput]
  , "parse integer success (positive)" ~: stripPos (parse integer "42 foo") ~?= Right (42, " foo")
  , "parse integer success (negative)" ~: stripPos (parse integer "-42 foo") ~?= Right (-42, " foo")
  , "parse integer failure (dash only)" ~: parse integer "- foo" ~?= Left [Unexpected' "-"]
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
  , "parse ident success" ~: stripPos (parse ident "foo123") ~?= Right ("foo123", "")
  , "parse ident failure (uppercase start)" ~: parse ident "Foo123" ~?= Left [Unexpected' "F"]
  , "parse ident failure (digit start)" ~: parse ident "123foo" ~?= Left [Unexpected' "1"]
  , "parse nat success" ~: stripPos (parse nat "123") ~?= Right (123, "")
  , "parse nat success with remainder" ~: stripPos (parse nat "456abc") ~?= Right (456, "abc")
  , "parse nat failure (no digits)" ~: parse nat "abc" ~?= Left [Unexpected' "a"]
  , "parse int success (positive)" ~: stripPos (parse int "123") ~?= Right (123, "")
  , "parse int success (negative)" ~: stripPos (parse int "-456") ~?= Right (-456, "")
  , "parse int failure (invalid negative)" ~: parse int "-abc" ~?= Left [Unexpected' "-"]
  -- Look ahead parsers
  , "parse lookAheadMulti success" ~: stripPos (parse (lookAheadMulti 3) "hello") ~?= Right ("hel", "hello")
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
  , "parse delimList with spaces" ~: stripPos (parse (delimList ',' identifier) "foo, bar, baz") ~?= Right (["foo", "bar", "baz"], "")
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

  -- Symbol parser with special characters
  , "parse identWith success (letter first)" ~: stripPos (parse (identWith ['_', '$']) "var_name$") ~?= Right ("var_name$", "")
  , "parse identWith success (special first)" ~: stripPos (parse (identWith ['_', '$']) "_private123") ~?= Right ("_private123", "")
  , "parse identWith failure (digit first)" ~: case parse (identWith ['_']) ("123var" :: Text) of
      Left errs -> assertBool "Should have errors" (not $ null errs)
      Right _ -> assertFailure "Should have failed"
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

  -- =====================================================================
  -- Position tracking tests
  -- initPos is Pos 1 1, advancePos increments col, newline resets to next line col 1
  -- =====================================================================

  -- Single character: starts at Pos 1 1, after consuming 'a' -> Pos 1 2
  , "pos: single char" ~:
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
  -- ident position: "foo" is 3 chars on line 1
  , "pos: ident" ~:
    getPosFromResult (parse ident "foo rest") @?= Just (Pos 1 4)
  -- nat position: "123" is 3 chars
  , "pos: nat" ~:
    getPosFromResult (parse nat "123rest") @?= Just (Pos 1 4)
  -- int negative: "-42" is 3 chars
  , "pos: negative int" ~:
    getPosFromResult (parse int "-42rest") @?= Just (Pos 1 4)
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
      digs   = T.takeWhile isDigit tinput
      rest   = T.dropWhile isDigit tinput
  in stripPos (parse (pTakeWhile isDigit) tinput) === Right (digs, rest)

prop_takeAll :: String -> Property
prop_takeAll input =
  let t = T.pack input in stripPos (parse takeAll t) === Right (t, T.empty)

prop_parseString_success :: String -> String -> Property
prop_parseString_success target input =
  T.pack target `T.isPrefixOf` T.pack input ==>
    let tt = T.pack target
        ti = T.pack input
    in stripPos (parse (string tt) ti) === Right (tt, T.drop (T.length tt) ti)

prop_parseNat_success :: Property
prop_parseNat_success = forAll (choose (0, 999999)) $ \n ->
  let str = T.pack (show n) in stripPos (parse nat str) === Right (n, T.empty)

prop_parseInt_success :: Property
prop_parseInt_success = forAll (choose (-999999, 999999)) $ \n ->
  let str = T.pack (show n) in stripPos (parse int str) === Right (n, T.empty)

-- Test choice with random parsers
prop_choice_success :: Property
prop_choice_success = forAll (elements ["foo", "bar", "baz"]) $ \target ->
  let tt = T.pack target
  in stripPos (parse (choice (map (string . T.pack) ["foo", "bar", "baz"])) (T.pack (target ++ "rest"))) === Right (tt, "rest")

main :: IO ()
main = do
  putStrLn "Running Enhanced HUnit tests..."
  cnts <- runTestTT hunitTests
  putStrLn $ "\nHUnit Results: " ++ show cnts

  putStrLn "\nRunning QuickCheck tests..."

  putStrLn "Testing basic parsers..."
  quickCheck prop_parseItem_nonEmpty
  quickCheck prop_parseItem_empty

  putStrLn "Testing character parsers..."
  quickCheck prop_parseDigit_success
  quickCheck prop_parseDigit_failure
  quickCheck prop_parseLetter_success
  quickCheck prop_parseLetter_failure
  quickCheck prop_parseAlphanum_success
  quickCheck prop_parseAlphanum_failure
  quickCheck prop_parseLower_success
  quickCheck prop_parseLower_failure
  quickCheck prop_parseUpper_success
  quickCheck prop_parseUpper_failure

  putStrLn "Testing lookAhead..."
  quickCheck prop_parseLookAhead_nonEmpty
  quickCheck prop_parseLookAhead_empty

  putStrLn "Testing takeUntil..."
  quickCheck prop_parseTakeUntil_success
  quickCheck prop_parseTakeUntil_failure
  quickCheck prop_parseTakeUntil'_success
  quickCheck prop_parseTakeUntil'_failure

  putStrLn "Testing pTakeWhile..."
  quickCheck prop_parseTakeWhile

  putStrLn "Testing takeAll..."
  quickCheck prop_takeAll

  putStrLn "Testing EOF..."
  quickCheck prop_parseEOF_success
  quickCheck prop_parseEOF_failure

  putStrLn "Testing string parsing..."
  quickCheck prop_parseString_success

  putStrLn "Testing number parsing..."
  quickCheck prop_parseNat_success
  quickCheck prop_parseInt_success

  putStrLn "Testing choice..."
  quickCheck prop_choice_success

  putStrLn "\nAll tests completed!"
