{-# LANGUAGE OverloadedStrings #-}

module Main where

import MiniParser.Base
import TestHelpers (stripPos, test, reportResults)
import Data.Text (Text)
import qualified MiniParser.Comments.C as CC

-- Token/identifier/symbol that use C comments instead of the default
cToken :: Parser a -> Parser a
cToken p = do
  CC.comments
  p

cIdentifier :: Parser Text
cIdentifier = cToken ident

cSymbol :: Text -> Parser Text
cSymbol xs = cToken (string xs)

cCharacter :: Char -> Parser Char
cCharacter = cToken . char

main :: IO ()
main = do
  putStrLn "C comment tests..."
  results <- sequence
    [ -- pELComment
      test "EL comment basic"
        (stripPos $ parse CC.pELComment "// a comment\nrest")
        (Right (" a comment", "rest"))
    , test "EL comment empty"
        (stripPos $ parse CC.pELComment "//\nrest")
        (Right ("", "rest"))
    , test "EL comment with cr-lf"
        (stripPos $ parse CC.pELComment "// comment\r\nrest")
        (Right (" comment", "rest"))
    , test "EL comment with slashes in body"
        (stripPos $ parse CC.pELComment "// foo // bar\nrest")
        (Right (" foo // bar", "rest"))
      -- pInlineComment
    , test "inline comment basic"
        (stripPos $ parse CC.pInlineComment "/* a comment */ rest")
        (Right (" a comment ", " rest"))
    , test "inline comment multiline"
        (stripPos $ parse CC.pInlineComment "/* line1\nline2 */ rest")
        (Right (" line1\nline2 ", " rest"))
    , test "inline comment with slashes"
        (stripPos $ parse CC.pInlineComment "/* a/b/c */ rest")
        (Right (" a/b/c ", " rest"))
    , test "inline comment with stars"
        (stripPos $ parse CC.pInlineComment "/* a * b */ rest")
        (Right (" a * b ", " rest"))
      -- comments (combined whitespace + comment stripping)
    , test "comments strips whitespace"
        (stripPos $ parse CC.comments "   \t\n  rest")
        (Right ((), "rest"))
    , test "comments strips EL comment"
        (stripPos $ parse CC.comments "// comment\nrest")
        (Right ((), "rest"))
    , test "comments strips inline comment"
        (stripPos $ parse CC.comments "/* comment */ rest")
        (Right ((), "rest"))
    , test "comments strips mixed"
        (stripPos $ parse CC.comments "// line\n/* inline */ rest")
        (Right ((), "rest"))
    , test "comments strips multiple inline"
        (stripPos $ parse CC.comments "/* a */ /* b */ rest")
        (Right ((), "rest"))
    , test "comments strips multiple EL"
        (stripPos $ parse CC.comments "// line1\n// line2\nrest")
        (Right ((), "rest"))
    , test "comments on empty input"
        (stripPos $ parse CC.comments ("" :: Text))
        (Right ((), ""))
    , test "comments no-op on plain text"
        (stripPos $ parse CC.comments "rest")
        (Right ((), "rest"))
      -- Integration: token/identifier/symbol with C comments
    , test "identifier after EL comment"
        (stripPos $ parse cIdentifier "// a comment\nfoo rest")
        (Right ("foo", " rest"))
    , test "symbol after inline comment"
        (stripPos $ parse (cSymbol "==") "/* comment */ == rest")
        (Right ("==", " rest"))
    , test "two tokens with comments between"
        (let p = do { a <- cToken (string "foo"); b <- cToken (string "bar"); return (a,b) }
         in stripPos $ parse p "/* c1 */ foo /* c2 */ bar rest")
        (Right (("foo","bar"), " rest"))
    , test "three identifiers with mixed comments"
        (stripPos $ parse (do a <- cIdentifier; b <- cIdentifier; c <- cIdentifier; return [a,b,c])
          "// header\nfoo /* mid */ bar baz")
        (Right (["foo", "bar", "baz"], ""))
    , test "character after comments"
        (stripPos $ parse (cCharacter '{') "/* open */ { body")
        (Right ('{', " body"))
    ]
  reportResults "C comment" results
