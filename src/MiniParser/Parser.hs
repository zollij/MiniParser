-- Parser is the main interface for using MiniParser.
-- This implementation was inspired by several sources:
-- Graham Hutton's "Programming in Haskell" book
-- https://serokell.io/blog/parser-combinators-in-haskell
-- It was hand coded with some advice on efficient implementation
-- (primarily correct usage of Text) as well as help with testing
-- coming from Claude code.

-- Parser is split from Base to break a circular dependency:
--   Comments.hs needs the Parser type (to define comments :: Parser ())
--   Parser.hs needs Comments.hs (to use comments inside token, identifierHaskell, etc.)
-- Base holds the types and primitives; this module re-exports Base and adds
-- higher-level parsers (token, identifierHaskell, etc.) that depend on Comments.
module MiniParser.Parser (
  module MiniParser.Base,
  token, decimal, hexidecimal, octal, binary, signed,
  symbol, character, delimList, trim, row, splitLines, splitLinesT,
  letters, identifierHaskell,
  float, expFloat
) where

import MiniParser.Base
import MiniParser.Comments (comments)
import Control.Applicative
import Data.Char (isAlpha, isSpace)
import qualified Data.Text as T

-- parse out a single token from the beginning of the Text stream.
-- This function will also throw away comments by using the comments parser
-- from MiniParser.Comments.
token :: Parser a -> Parser a
token p = do
  comments
  p

-- parse a Haskell identifier
identifierHaskell :: Parser T.Text
identifierHaskell = token identHaskell

-- parse an unsigned decimal (base 10) number.
-- Polymorphic over Num; specialize at the call site via a type annotation
-- when the context is ambiguous, e.g. `parse (decimal :: Parser Integer) s`.
decimal :: Num a => Parser a
decimal = token dec

-- parse a hexidecimal (base 16) number; starting with "0x"
hexidecimal :: Num a => Parser a
hexidecimal = do
  comments
  _ <- char '0'
  _ <- char 'x' <|> char 'X'
  hex

-- parse an octal (base 8) number; starting with "0o"
octal :: Num a => Parser a
octal = do
  comments
  _ <- char '0'
  _ <- char 'o' <|> char 'O'
  oct

-- parse a binary (base 2) number; starting with "0b"
binary :: Num a => Parser a
binary = do
  comments
  _ <- char '0'
  _ <- char 'b' <|> char 'B'
  bin

-- Combinator for a signed number.
-- Strips leading whitespace and comments, then accepts an optional '-' or
-- '+' that must be immediately followed (no whitespace) by the wrapped
-- parser. So:
--   * `signed decimal "  -42"`   -> -42    (whitespace before sign OK)
--   * `signed decimal "-42"`     -> -42
--   * `signed decimal "+42"`     ->  42    (explicit positive)
--   * `signed decimal "42"`      ->  42    (sign optional)
--   * `signed decimal "- 42"`    -> fails  (no whitespace between sign and digits)
--   * `signed decimal "  - 42"`  -> fails  (still rejects mid-space)
-- The same rules apply to `signed hexidecimal`, `signed octal`, and
-- `signed binary`.
signed :: Num a => Parser a -> Parser a
signed p = comments *> (negative <|> positive <|> p)
  where
    negative = char '-' *> noSpaceAhead *> (negate <$> p)
    positive = char '+' *> noSpaceAhead *> p

-- After consuming a sign, require the next character to not be whitespace.
-- Used by 'signed' to reject inputs like "- 42".
noSpaceAhead :: Parser Char
noSpaceAhead = do
  c <- lookAhead
  if isSpace c then empty else pure c

-- parse a string symbol
symbol :: T.Text -> Parser T.Text
symbol xs = token (string xs)

-- parse a character
character :: Char -> Parser Char
character = token . char

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

-- ── Floating-point parsers ────────────────────────────────
-- The raw `fp` parser lives in Base.hs alongside dec/hex/oct/bin. The
-- whitespace/comment-stripping wrappers below (float, expFloat) live here
-- because they call `token`.

-- floating point, eats comments. Default cap is 4 exponent digits, which
-- covers the entire Double range (~1e308). If you need a different cap, use
-- expFloat directly. Most users will want this parser.
float :: RealFrac f => Parser f
float = expFloat 4

-- floating point, eats comments
-- The Int argument is the maximum number of digits allowed in the exponent
-- portion of the input (e.g. expLen=4 accepts "1e9999" but rejects "1e10000").
-- The cap defends against DoS via huge intermediate Integers inside readFloat.
-- Most users won't need expFloat and will just use "float".
expFloat :: RealFrac f => Int -> Parser f
expFloat expLen = token $ fp expLen
