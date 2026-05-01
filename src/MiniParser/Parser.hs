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
  letters, identifierHaskell, float, expFloat, scientific, expScientific
) where

import MiniParser.Base
import MiniParser.Comments (comments)
import Control.Applicative
import Data.Char (isAlpha, isSpace)
import qualified Data.Text as T
import Data.Scientific (Scientific)

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

-- ── Numeric: scientific and floating-point parsers ────────────────────────
-- The raw 'sci' and 'fp' primitives live in Base.hs alongside dec/hex/oct/bin.
-- The whitespace/comment-stripping wrappers below (scientific, expScientific,
-- float, expFloat) live here because they call 'token'.
--
-- 'scientific'/'expScientific' are lenient (accept bare integer-shape input);
-- 'float'/'expFloat' are strict-fractional (reject bare integers, matching
-- Megaparsec's 'Text.Megaparsec.Char.Lexer.float'). See the doc strings on
-- 'sci' and 'fp' in Base.hs for the rationale.

-- | Decimal scientific-notation parser, eats leading whitespace and comments.
-- Lenient — accepts integer-shape input (@42@ → @Sci.scientific 42 0@) as
-- well as fractional and scientific forms. Returns the literal as
-- 'Data.Scientific.Scientific' (coefficient × 10^exp, exact). Default cap
-- on exponent length is 4 digits, which covers the entire Double range
-- (~1e308). For higher or lower caps, use 'expScientific'.
--
-- Use 'scientific' (or 'expScientific') when downstream code needs to
-- (a) preserve the user's exact literal across overflow checks,
-- (b) distinguish "@42@" from "@42.0@" via 'Sci.isInteger', or
-- (c) range-check via 'Sci.toBoundedInteger' before narrowing. The
-- 'float' family below is for source languages that lexically distinguish
-- integer literals from float literals.
scientific :: Parser Scientific
scientific = expScientific 4

-- | Like 'scientific' but with a caller-supplied exponent-length cap.
-- The cap defends against DoS via pathological inputs like @1e1000000000@.
-- Most users will want 'scientific' (which is @expScientific 4@).
expScientific :: Int -> Parser Scientific
expScientific = token . sci

-- | Floating-point parser, eats leading whitespace and comments. STRICT —
-- the input must contain a @.@ followed by digits, or an exponent
-- (@e@/@E@ optionally signed, followed by digits). Bare integer-shape
-- input fails. Default cap is 4 exponent digits (covers the entire
-- Double range, ~1e308); use 'expFloat' for a different cap.
--
-- Strict semantics match Megaparsec's
-- 'Text.Megaparsec.Char.Lexer.float'. They are intended for source
-- languages where @42@ and @42.0@ are lexically distinct tokens.
-- Callers parsing a language where any numeric form is acceptable
-- should use 'scientific' (or 'expScientific') instead, which is lenient.
--
-- Implementation: parses via the strict 'fp' primitive in Base.hs.
-- Narrowing to the target RealFloat uses 'Sci.toRealFloat' (IEEE
-- correctly-rounded conversion). Out-of-range inputs return @Infinity@
-- or @0@ per IEEE 754.
float :: RealFloat f => Parser f
float = expFloat 4

-- | Like 'float' but with a caller-supplied exponent-length cap. The cap
-- counts input *digits*, not the parsed value, so leading zeros count.
-- Most users won't need this and will just use 'float'.
expFloat :: RealFloat f => Int -> Parser f
expFloat = token . fp
