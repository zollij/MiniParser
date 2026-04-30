{-# LANGUAGE OverloadedStrings #-}

module Main where

import MiniParser.Base
import TestHelpers (stripPos, test, reportResults)
import Data.Text (Text)
import qualified MiniParser.Comments.Java as JV

-- Token/identifier/symbol that use Java comments
jvToken :: Parser a -> Parser a
jvToken p = do
  JV.comments
  p

jvIdentifier :: Parser Text
jvIdentifier = jvToken identHaskell

jvSymbol :: Text -> Parser Text
jvSymbol xs = jvToken (string xs)

main :: IO ()
main = do
  putStrLn "Java comment tests..."
  results <- sequence
    [ -- eolComment
      test "EL comment basic"
        (stripPos $ parse JV.eolComment "// a comment\nrest")
        (Right (" a comment", "rest"))
    , test "EL comment empty"
        (stripPos $ parse JV.eolComment "//\nrest")
        (Right ("", "rest"))
      -- inlineComment
    , test "inline comment basic"
        (stripPos $ parse JV.inlineComment "/* a comment */ rest")
        (Right (" a comment ", " rest"))
    , test "inline comment rejects Javadoc comment"
        (case parse JV.inlineComment ("/** api */" :: Text) of
           Left _ -> Right ("rejected" :: Text, "" :: Text)
           Right _ -> Left [CustomError "should have rejected"])
        (Right ("rejected", ""))
      -- javadocComment
    , test "Javadoc comment basic"
        (stripPos $ parse JV.javadocComment "/**\n * Description\n * @param x input\n * @return output\n */rest")
        (Right (["* Description", "* @param x input", "* @return output"], "rest"))
    , test "Javadoc comment single line"
        (stripPos $ parse JV.javadocComment "/** brief */rest")
        (Right (["brief"], "rest"))
    , test "Javadoc comment empty"
        (stripPos $ parse JV.javadocComment "/***/rest")
        (Right ([], "rest"))
      -- comments (combined whitespace + comment stripping)
    , test "comments strips whitespace"
        (stripPos $ parse JV.comments "   \t\n  rest")
        (Right ((), "rest"))
    , test "comments strips EL comment"
        (stripPos $ parse JV.comments "// comment\nrest")
        (Right ((), "rest"))
    , test "comments strips inline comment"
        (stripPos $ parse JV.comments "/* comment */ rest")
        (Right ((), "rest"))
    , test "comments strips Javadoc comment"
        (stripPos $ parse JV.comments "/** doc */ rest")
        (Right ((), "rest"))
    , test "comments strips all three types"
        (stripPos $ parse JV.comments "// line\n/* inline */ /** javadoc */ rest")
        (Right ((), "rest"))
    , test "comments strips all three reversed"
        (stripPos $ parse JV.comments "/** javadoc */ /* inline */ // line\nrest")
        (Right ((), "rest"))
    , test "comments on empty input"
        (stripPos $ parse JV.comments ("" :: Text))
        (Right ((), ""))
    , test "comments no-op on plain text"
        (stripPos $ parse JV.comments "rest")
        (Right ((), "rest"))
      -- Integration: token/identifier/symbol with Java comments
    , test "identifier after EL comment"
        (stripPos $ parse jvIdentifier "// a comment\nfoo rest")
        (Right ("foo", " rest"))
    , test "symbol after inline comment"
        (stripPos $ parse (jvSymbol "=") "/* comment */ = rest")
        (Right ("=", " rest"))
    , test "identifier after Javadoc comment"
        (stripPos $ parse jvIdentifier "/** doc */  myFunc rest")
        (Right ("myFunc", " rest"))
    , test "two tokens with comments between"
        (let p = do { a <- jvToken (string "class"); b <- jvIdentifier; return (a,b) }
         in stripPos $ parse p "/** Main class */ class /* name */ myClass {")
        (Right (("class","myClass"), " {"))
    , test "three identifiers with mixed comments"
        (stripPos $ parse (do a <- jvIdentifier; b <- jvIdentifier; c <- jvIdentifier; return [a,b,c])
          "// header\nfoo /* mid */ bar /** doc */ baz")
        (Right (["foo", "bar", "baz"], ""))
    ]
  reportResults "Java comment" results
