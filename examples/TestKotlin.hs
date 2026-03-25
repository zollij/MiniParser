{-# LANGUAGE OverloadedStrings #-}

module Main where

import MiniParser.Base
import TestHelpers (stripPos, test, reportResults)
import Data.Text (Text)
import qualified MiniParser.Comments.Kotlin as KT

-- Token/identifier/symbol that use Kotlin comments
ktToken :: Parser a -> Parser a
ktToken p = do
  KT.comments
  p

ktIdentifier :: Parser Text
ktIdentifier = ktToken ident

ktSymbol :: Text -> Parser Text
ktSymbol xs = ktToken (string xs)

main :: IO ()
main = do
  putStrLn "Kotlin comment tests..."
  results <- sequence
    [ -- pELComment
      test "EL comment basic"
        (stripPos $ parse KT.pELComment "// a comment\nrest")
        (Right (" a comment", "rest"))
    , test "EL comment empty"
        (stripPos $ parse KT.pELComment "//\nrest")
        (Right ("", "rest"))
      -- pBlockComment (non-nested)
    , test "block comment basic"
        (stripPos $ parse KT.pBlockComment "/* a comment */ rest")
        (Right (" a comment ", " rest"))
    , test "block comment multiline"
        (stripPos $ parse KT.pBlockComment "/* line1\nline2 */ rest")
        (Right (" line1\nline2 ", " rest"))
    , test "block comment rejects KDoc"
        (case parse KT.pBlockComment ("/** kdoc */" :: Text) of
           Left _ -> Right ("rejected" :: Text, "" :: Text)
           Right _ -> Left [CustomError "should have rejected"])
        (Right ("rejected", ""))
      -- pBlockComment (nested)
    , test "block comment nested simple"
        (stripPos $ parse KT.pBlockComment "/* outer /* inner */ still */ rest")
        (Right (" outer /* inner */ still ", " rest"))
    , test "block comment nested double"
        (stripPos $ parse KT.pBlockComment "/* a /* b /* c */ d */ e */ rest")
        (Right (" a /* b /* c */ d */ e ", " rest"))
    , test "block comment nested empty inner"
        (stripPos $ parse KT.pBlockComment "/* outer /**/ end */ rest")
        (Right (" outer /**/ end ", " rest"))
    , test "block comment nested multiline"
        (stripPos $ parse KT.pBlockComment "/* line1\n/* nested\ncomment */\nline4 */ rest")
        (Right (" line1\n/* nested\ncomment */\nline4 ", " rest"))
      -- pKDocComment
    , test "KDoc comment basic"
        (stripPos $ parse KT.pKDocComment "/**\n * Description\n * @param x input\n * @return output\n */rest")
        (Right (["* Description", "* @param x input", "* @return output"], "rest"))
    , test "KDoc comment single line"
        (stripPos $ parse KT.pKDocComment "/** brief */rest")
        (Right (["brief"], "rest"))
    , test "KDoc comment empty"
        (stripPos $ parse KT.pKDocComment "/***/rest")
        (Right ([], "rest"))
      -- comments (combined whitespace + comment stripping)
    , test "comments strips whitespace"
        (stripPos $ parse KT.comments "   \t\n  rest")
        (Right ((), "rest"))
    , test "comments strips EL comment"
        (stripPos $ parse KT.comments "// comment\nrest")
        (Right ((), "rest"))
    , test "comments strips block comment"
        (stripPos $ parse KT.comments "/* comment */ rest")
        (Right ((), "rest"))
    , test "comments strips KDoc comment"
        (stripPos $ parse KT.comments "/** doc */ rest")
        (Right ((), "rest"))
    , test "comments strips nested block comment"
        (stripPos $ parse KT.comments "/* outer /* inner */ end */ rest")
        (Right ((), "rest"))
    , test "comments strips all three types"
        (stripPos $ parse KT.comments "// line\n/* block */ /** kdoc */ rest")
        (Right ((), "rest"))
    , test "comments strips all three reversed"
        (stripPos $ parse KT.comments "/** kdoc */ /* block */ // line\nrest")
        (Right ((), "rest"))
    , test "comments on empty input"
        (stripPos $ parse KT.comments ("" :: Text))
        (Right ((), ""))
    , test "comments no-op on plain text"
        (stripPos $ parse KT.comments "rest")
        (Right ((), "rest"))
      -- Integration: token/identifier/symbol with Kotlin comments
    , test "identifier after EL comment"
        (stripPos $ parse ktIdentifier "// a comment\nfoo rest")
        (Right ("foo", " rest"))
    , test "symbol after block comment"
        (stripPos $ parse (ktSymbol "=") "/* comment */ = rest")
        (Right ("=", " rest"))
    , test "identifier after KDoc comment"
        (stripPos $ parse ktIdentifier "/** doc */  myFunc rest")
        (Right ("myFunc", " rest"))
    , test "identifier after nested block comment"
        (stripPos $ parse ktIdentifier "/* outer /* inner */ end */ myFunc rest")
        (Right ("myFunc", " rest"))
    , test "two tokens with comments between"
        (let p = do { a <- ktToken (string "class"); b <- ktIdentifier; return (a,b) }
         in stripPos $ parse p "/** Main class */ class /* name */ myClass {")
        (Right (("class","myClass"), " {"))
    , test "three identifiers with mixed comments"
        (stripPos $ parse (do a <- ktIdentifier; b <- ktIdentifier; c <- ktIdentifier; return [a,b,c])
          "// header\nfoo /* mid /* nested */ end */ bar /** doc */ baz")
        (Right (["foo", "bar", "baz"], ""))
    ]
  reportResults "Kotlin comment" results
