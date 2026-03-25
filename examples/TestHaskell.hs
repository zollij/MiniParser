{-# LANGUAGE OverloadedStrings #-}

module Main where

import MiniParser.Base
import TestHelpers (stripPos, test, reportResults)
import Data.Text (Text)
import qualified MiniParser.Comments.Haskell as HC

-- Token/identifier/symbol that use Haskell comments
hToken :: Parser a -> Parser a
hToken p = do
  HC.comments
  p

hIdentifier :: Parser Text
hIdentifier = hToken ident

hSymbol :: Text -> Parser Text
hSymbol xs = hToken (string xs)

main :: IO ()
main = do
  putStrLn "Haskell comment tests..."
  results <- sequence
    [ -- eolComment
      test "EL comment basic"
        (stripPos $ parse HC.eolComment "-- a comment\nrest")
        (Right (" a comment", "rest"))
    , test "EL comment empty"
        (stripPos $ parse HC.eolComment "--\nrest")
        (Right ("", "rest"))
    , test "EL comment with cr-lf"
        (stripPos $ parse HC.eolComment "-- comment\r\nrest")
        (Right (" comment", "rest"))
    , test "EL comment with dashes in body"
        (stripPos $ parse HC.eolComment "-- foo -- bar\nrest")
        (Right (" foo -- bar", "rest"))
      -- blockComment
    , test "block comment basic"
        (stripPos $ parse HC.blockComment "{- a block comment -} rest")
        (Right (" a block comment ", " rest"))
    , test "block comment multiline"
        (stripPos $ parse HC.blockComment "{- line1\nline2 -} rest")
        (Right (" line1\nline2 ", " rest"))
      -- Nested block comments
    , test "block comment nested simple"
        (stripPos $ parse HC.blockComment "{- outer {- inner -} still -} rest")
        (Right (" outer {- inner -} still ", " rest"))
    , test "block comment nested double"
        (stripPos $ parse HC.blockComment "{- a {- b {- c -} d -} e -} rest")
        (Right (" a {- b {- c -} d -} e ", " rest"))
    , test "block comment nested empty inner"
        (stripPos $ parse HC.blockComment "{- outer {--} end -} rest")
        (Right (" outer {--} end ", " rest"))
    , test "block comment nested multiline"
        (stripPos $ parse HC.blockComment "{- line1\n{- nested\ncomment -}\nline4 -} rest")
        (Right (" line1\n{- nested\ncomment -}\nline4 ", " rest"))
    , test "comments strips nested block comment"
        (stripPos $ parse HC.comments "{- outer {- inner -} end -} rest")
        (Right ((), "rest"))
      -- comments (combined whitespace + comment stripping)
    , test "comments strips whitespace"
        (stripPos $ parse HC.comments "   \t\n  rest")
        (Right ((), "rest"))
    , test "comments strips EL comment"
        (stripPos $ parse HC.comments "-- comment\nrest")
        (Right ((), "rest"))
    , test "comments strips block comment"
        (stripPos $ parse HC.comments "{- block -} rest")
        (Right ((), "rest"))
    , test "comments strips mixed"
        (stripPos $ parse HC.comments "-- line\n{- block -} rest")
        (Right ((), "rest"))
    , test "comments strips multiple blocks"
        (stripPos $ parse HC.comments "{- a -} {- b -} rest")
        (Right ((), "rest"))
    , test "comments on empty input"
        (stripPos $ parse HC.comments ("" :: Text))
        (Right ((), ""))
    , test "comments no-op on plain text"
        (stripPos $ parse HC.comments "rest")
        (Right ((), "rest"))
      -- Integration: token/identifier/symbol with Haskell comments
    , test "identifier after EL comment"
        (stripPos $ parse hIdentifier "-- a comment\nfoo rest")
        (Right ("foo", " rest"))
    , test "symbol after block comment"
        (stripPos $ parse (hSymbol "=>") "{- block -} => rest")
        (Right ("=>", " rest"))
    , test "two identifiers with mixed comments"
        (stripPos $ parse (do a <- hIdentifier; b <- hIdentifier; return (a, b))
          "-- line comment\nfoo {- block -} bar")
        (Right (("foo", "bar"), ""))
    , test "token with string"
        (stripPos $ parse (hToken (string "hello")) "-- greeting\nhello world")
        (Right ("hello", " world"))
    , test "three identifiers"
        (stripPos $ parse (do a <- hIdentifier; b <- hIdentifier; c <- hIdentifier; return [a,b,c])
          "{- x -} foo -- mid\nbar {- y -} baz")
        (Right (["foo", "bar", "baz"], ""))
    ]
  reportResults "Haskell comment" results
