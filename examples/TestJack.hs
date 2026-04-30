{-# LANGUAGE OverloadedStrings #-}

module Main where

import MiniParser.Base
import TestHelpers (stripPos, test, reportResults)
import Data.Text (Text)
import qualified MiniParser.Comments.Jack as JC

-- Token/identifier/symbol that use Jack comments
jToken :: Parser a -> Parser a
jToken p = do
  JC.comments
  p

jIdentifier :: Parser Text
jIdentifier = jToken identHaskell

jSymbol :: Text -> Parser Text
jSymbol xs = jToken (string xs)

main :: IO ()
main = do
  putStrLn "Jack comment tests..."
  results <- sequence
    [ -- eolComment
      test "EL comment basic"
        (stripPos $ parse JC.eolComment "// a comment\nrest")
        (Right (" a comment", "rest"))
    , test "EL comment empty"
        (stripPos $ parse JC.eolComment "//\nrest")
        (Right ("", "rest"))
      -- inlineComment
    , test "inline comment basic"
        (stripPos $ parse JC.inlineComment "/* a comment */ rest")
        (Right (" a comment ", " rest"))
    , test "inline comment rejects API comment"
        (case parse JC.inlineComment ("/** api */" :: Text) of
           Left _ -> Right ("rejected" :: Text, "" :: Text)
           Right _ -> Left [CustomError "should have rejected"])
        (Right ("rejected", ""))
      -- apiComment
    , test "API comment basic"
        (stripPos $ parse JC.apiComment "/**\n * Description\n * @param x input\n * @return output\n */rest")
        (Right (["* Description", "* @param x input", "* @return output"], "rest"))
    , test "API comment single line"
        (stripPos $ parse JC.apiComment "/** brief */rest")
        (Right (["brief"], "rest"))
    , test "API comment empty"
        (stripPos $ parse JC.apiComment "/***/rest")
        (Right ([], "rest"))
      -- comments (combined whitespace + comment stripping)
    , test "comments strips whitespace"
        (stripPos $ parse JC.comments "   \t\n  rest")
        (Right ((), "rest"))
    , test "comments strips EL comment"
        (stripPos $ parse JC.comments "// comment\nrest")
        (Right ((), "rest"))
    , test "comments strips inline comment"
        (stripPos $ parse JC.comments "/* comment */ rest")
        (Right ((), "rest"))
    , test "comments strips API comment"
        (stripPos $ parse JC.comments "/** doc */ rest")
        (Right ((), "rest"))
    , test "comments strips all three types"
        (stripPos $ parse JC.comments "// line\n/* inline */ /** api */ rest")
        (Right ((), "rest"))
    , test "comments strips all three reversed"
        (stripPos $ parse JC.comments "/** api */ /* inline */ // line\nrest")
        (Right ((), "rest"))
    , test "comments on empty input"
        (stripPos $ parse JC.comments ("" :: Text))
        (Right ((), ""))
    , test "comments no-op on plain text"
        (stripPos $ parse JC.comments "rest")
        (Right ((), "rest"))
      -- Integration: token/identifier/symbol with Jack comments
    , test "identifier after EL comment"
        (stripPos $ parse jIdentifier "// a comment\nfoo rest")
        (Right ("foo", " rest"))
    , test "symbol after inline comment"
        (stripPos $ parse (jSymbol "=") "/* comment */ = rest")
        (Right ("=", " rest"))
    , test "identifier after API comment"
        (stripPos $ parse jIdentifier "/** doc */  myFunc rest")
        (Right ("myFunc", " rest"))
    , test "two tokens with comments between"
        (let p = do { a <- jToken (string "class"); b <- jIdentifier; return (a,b) }
         in stripPos $ parse p "/** Main class */ class /* name */ myClass {")
        (Right (("class","myClass"), " {"))
    , test "three identifiers with mixed comments"
        (stripPos $ parse (do a <- jIdentifier; b <- jIdentifier; c <- jIdentifier; return [a,b,c])
          "// header\nfoo /* mid */ bar /** doc */ baz")
        (Right (["foo", "bar", "baz"], ""))
    ]
  reportResults "Jack comment" results
