-- Parser is the main interface for using MiniParser.
-- This implementation was inspired by several sources:
-- Graham Hutton's "Programming in Haskell" book
-- https://serokell.io/blog/parser-combinators-in-haskell
-- It was hand coded with some advice on efficient implementation
-- (primarily correct usage of Text) as well as help with testing
-- coming from Claude code.

-- Parser is split from Base to break a circular dependency:
--   Comments.hs needs the Parser type (to define comments :: Parser ())
--   Parser.hs needs Comments.hs (to use comments inside token, identifier, etc.)
-- Base holds the types and primitives; this module re-exports Base and adds
-- higher-level parsers (token, identifier, etc.) that depend on Comments.
module MiniParser.Parser (
  -- Re-export everything from Base
  module MiniParser.Base,
  -- Higher-level parsers that use comments
  token, identifier, natural, integer, symbol, character,
  delimList, trim, row, splitLines, splitLinesT, digits,
  letters
) where

import MiniParser.Base
import MiniParser.Comments (comments)
import Control.Applicative
import Data.Char (isDigit, isAlpha)
import qualified Data.Text as T

-- parse out a single token from the beginning of the Text stream.
-- This function will also throw away comments by using the comments parser
-- from MiniParser.Comments.
token :: Parser a -> Parser a
token p = do
  comments
  p

identifier :: Parser T.Text
identifier = token ident

natural :: Parser Int
natural = token nat

integer :: Parser Int
integer = token int

symbol :: T.Text -> Parser T.Text
symbol xs = token (string xs)

character :: Char -> Parser Char
character = token . char

-- efficient digits implementation, using Text instead of [Char]
-- use this instead of "many digit"
digits :: Parser T.Text
digits = pTakeWhile1 isDigit

-- efficient letters implementation, using Text instead of [Char]
-- use this instead of "many letter"
letters :: Parser T.Text
letters = pTakeWhile1 isAlpha

-- get a list of items delimited with a single char delimiter
delimList :: Char -> Parser a -> Parser [a]
delimList delim p = sepBy p (character delim)

-- trim initial spaces, trailing spaces and trailing EOL characters from a line
trim :: Parser T.Text
trim = do
  comments  -- strips leading whitespace and comments (pDiscard includes space)
  -- read until we see EOL or EOF
  l <- pTakeWhile (\x -> x /= '\n' && x /= '\r')
  -- at this point l may contain trailing spaces; remove them here:
  let l' = T.dropWhileEnd (== ' ') l
  -- consume any EOL characters
  _ <- optional $ pTakeWhile (\x -> x == '\n' || x == '\r')
  if T.null l' then empty else return l'

-- get a row of input, each row separated by '\n' or '\r\n'
row :: Parser T.Text
row = (eof *> empty) <|> trim

splitLines :: Parser [T.Text]
splitLines = many row

-- The lines function in Data.Text doesn't deal with '\r' so
-- we'll write our own here.
splitLinesT :: T.Text -> [T.Text]
splitLinesT inp =
  case parse splitLines inp of
    Right (s, _pos, _text)  -> s
    Left errs               -> error $ errorsToString errs
