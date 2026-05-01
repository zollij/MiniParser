{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

-- Base is split from Parser to break a circular dependency:
--   Comments.hs needs the Parser type (to define comments :: Parser ())
--   Parser.hs needs Comments.hs (to use comments inside token, identifierHaskell, etc.)
-- Base holds the types and primitives that both modules need.
module MiniParser.Base where

import Control.Applicative
import Data.Char
import Data.List (foldl')
import Data.Text (Text)
import qualified Data.Text as T
import Data.Scientific (Scientific)
import qualified Data.Scientific as Sci

data Error =
  EndOfInput                  |  -- unexpected EOF
  Unexpected !String !String  |  -- Unexpected <expect> <actual>
  Unexpected' !String         |  -- Unexpected <actual>
  CustomError !String         |  -- other types of errors
  Empty                       |  -- used by Alternative to deal with <|>
  ExpectedEndOfFile !Char        -- used by eof parser
  deriving (Eq, Show)

-- Basic definitions
-- At the most basic level, a Parser can be though of as
-- a function that takes Text as input and
-- outputs the desired (parsed) data item (of type a)
-- and the remainder of the stream.
-- This simplistic view has two additional, related features
-- added to it for convenience:
-- * errors -- for when the parse fails
-- * position -- for information about where we are in the parse,
--   primarily for identifying where in a parsed file we found an error.

-- originally (basic level), we defined Parser as:
--   newtype Parser = P (String -> (a, String))
-- but because using String has some potentially severe
-- performance issues, we switch to:
--   newtype Parser = P (Text -> (a, Text))
-- This lacks any type of error handling, so we evolved to:
--   newtype Parser a = P (Text -> Either [Error] (a, Text))
-- And then we add positional information:
data PState = PState !Pos !Text
newtype Parser a = P (PState -> Either [Error] (a, PState))

-- Pos (row, column)
data Pos = Pos !Int !Int deriving (Eq, Show)

-- Our positional info contains both line and column
-- starting at position (line=1, column=1). You can
-- customize this for whatever your environment expects,
-- e.g. emacs uses (line=1, column=0)
initPos :: Pos                                            
initPos = Pos 1 1

-- advance the position by a single char within the Text stream / file
advanceChar :: Char -> Pos -> Pos
advanceChar '\n' (Pos l _) = Pos (l + 1) 1
advanceChar _    (Pos l c) = Pos l (c + 1)

-- advance position over a chunk of Text
-- Processes line-at-a-time using T.takeWhile/T.drop (available in MHS)
-- to avoid allocating an intermediate [Char] list.
advanceText :: Text -> Pos -> Pos
advanceText t (Pos l c)
  | T.null t  = Pos l c
  | otherwise =
      let beforeNL = T.takeWhile (/= '\n') t
          rest     = T.drop (T.length beforeNL) t
      in case T.uncons rest of
           Nothing      -> Pos l (c + T.length beforeNL)
           Just (_, rs) -> advanceText rs (Pos (l + 1) 1)

-- return (row, col) of the input Text
getPos :: Parser (Int, Int)
getPos = P $ \ps@(PState (Pos row col) _inp) ->
  Right ((row, col), ps)

-- "parse" unwraps the Parser's function from the Parser's
-- data constructor & runs the function on the input Text.
-- It hides PState internals for backwards compatibility.
parse :: Parser a -> Text -> Either [Error] (a, Pos, Text)
parse (P p) inp =
  case p (PState initPos inp) of
    Left err                      -> Left err
    Right (a, PState pos' rest)   -> Right (a, pos', rest)

instance Functor Parser where
   -- fmap :: (a -> b) -> Parser a -> Parser b
  fmap f (P p) = P
    (\st ->
       case p st of
         Left err          -> Left err
         Right (out, st')  -> Right (f out, st'))

instance Applicative Parser where
  -- pure :: a -> Parser a
  pure v = P (\st -> Right (v, st))
  -- <*> :: Parser (a -> b) -> Parser a -> Parser b
  P f <*> P g = P
    (\st ->
       case f st of
         Left err -> Left err
         Right (f', st') ->
           case g st' of
             Left err -> Left err
             Right (out, st'') -> Right (f' out, st'')
    )

instance Monad Parser where
   -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  P p >>= k = P
    (\st ->
       case p st of
         Left err         -> Left err
         Right (out, st') -> let P p' = k out in p' st')

-- Making choices using Alternative.
-- Note that this implementation of <|> backtracks.
-- Also, we've intentionally dropped the error list for p in "p <|> q"
-- for efficiency reasons. If this is needed, re-enable the following
-- code and delete the second implementation that removes this.
-- instance Alternative Parser where
--   empty = P (\_ -> Left [Empty])
--   -- (<|>) :: Parser a -> Parser a -> Parser a
--   P p <|> P q = P
--     (\inp -> case p inp of
--                Left err ->
--                  case q inp of
--                    Left err' -> Left $ err <> err'
--                    Right (out, rest)  -> Right (out, rest)
--                Right (out, rest)  -> Right (out, rest))
instance Alternative Parser where
  empty = P (\_ -> Left [Empty])
  -- (<|>) :: Parser a -> Parser a -> Parser a
  P p <|> P q = P
    (\st ->
       case p st of
         Left _ -> q st
         Right r -> Right r)

-- MiniParser's <|> always backtracks, so try is a no-op.
-- It exists for compatibility with Parsec-style grammars.
try :: Parser a -> Parser a
try = id

-- "item" grabs a single Char off the front of the parse stream
-- and returns it (along with the remainder of the stream.)
item :: Parser Char
item = P
  (\(PState pos inp) ->
      case T.uncons inp of
        Nothing     -> Left [EndOfInput]
        Just (x,xs) -> Right (x, PState (advanceChar x pos) xs))

-- satisfy a particular predicate
satisfy :: (Char -> Bool) -> Parser Char
satisfy test = P
  (\(PState pos inp) ->
      case T.uncons inp of
        Nothing -> Left [EndOfInput]
        Just (x, xs)
          | test x     -> Right (x, PState (advanceChar x pos) xs)
          | otherwise  -> Left [Unexpected' [x]]
    )

digit :: Parser Char
digit = satisfy isDigit

lower :: Parser Char
lower = satisfy isLower

upper :: Parser Char
upper = satisfy isUpper

letter :: Parser Char
letter = satisfy isAlpha

alphanum :: Parser Char
alphanum = satisfy isAlphaNum

char :: Char -> Parser Char
char x = satisfy (== x)

string :: Text -> Parser Text
string s = P $ \(PState pos inp) ->
  if s `T.isPrefixOf` inp
  then Right (s, PState (advanceText s pos) (T.drop (T.length s) inp))
  else Left [Unexpected (T.unpack s) (T.unpack (T.take (T.length s) inp))]

-- parse a Haskell style identifier
-- for non-Haskell languages, you will need to define your own identifier parser
identHaskell :: Parser Text
identHaskell = P $ \(PState pos inp) ->
  case T.uncons inp of  -- get first Char
    Nothing -> Left [EndOfInput]
    Just (c, rest)
      | isLower c ->  -- make sure first Char is lower case
          let
            more = T.takeWhile isAlphaNum rest
            whole = T.cons c more
          in Right (whole, PState (advanceText whole pos) (T.drop (T.length more) rest))
      | otherwise -> Left [Unexpected' [c]]

-- parse a decimal (base 10) number
-- Polymorphic over Num, so it can be specialized to Int, Integer, Word, etc.
-- at the call site. A type annotation may be needed if the context doesn't
-- pin down the result type: e.g. `parse (dec :: Parser Integer) "123"`.
dec :: Num a => Parser a
dec = digs isDigit 10

-- parse a hexidecimal (base 16) number
-- hex assumes we've already parsed the "0x"
hex :: Num a => Parser a
hex = digs isHexDigit 16

bin :: Num a => Parser a
bin = digs (\c -> c == '0' || c == '1') 2

oct :: Num a => Parser a
oct = digs isOctDigit 8

-- efficient digits implementation, using Text instead of [Char]
-- use this instead of "many digit"
digits :: Parser Text
digits = pTakeWhile1 isDigit

-- ── Numeric primitives: scientific and floating-point ──────────────────
-- These live in Base alongside dec/hex/oct/bin because they don't depend
-- on comment handling. The whitespace/comment-stripping wrappers (scientific,
-- expScientific, float, expFloat) live in MiniParser.Parser.
--
-- The shape:
--   - 'sci'  is LENIENT:  accepts integer-shape input (@42@ → 42).
--   - 'fp'   is STRICT:   requires '.' digits or 'e'/'E' digits, matching
--                         Megaparsec's 'Text.Megaparsec.Char.Lexer.float'.
--                         Bare integer-shape input fails so the user reaches
--                         for 'sci'/'scientific' explicitly when they want
--                         lenient parsing.
-- Both share the digit-shape parser 'sciParts' below, so the cap discipline
-- and remainder behaviour stay in lockstep.

-- | Internal API which parses the component parts making up a scientific number.
-- Returns @(integerDigits, fractionalDigits, optionalExponent)@. Always
-- requires at least one integer digit; fractional and exponent parts are
-- both optional. The exponent-length cap (expCap) is enforced as a HARD failure.
-- That is, exceeding expCap rejects the whole input rather than backtracking.
--
-- sciParts is used by 'sci' (lenient) and 'fp' (strict). sciParts is not exported.
-- Users should call 'sci' or 'fp' instead.
sciParts :: Int -> Parser (Text, Text, Maybe Int)
sciParts expCap = do
  whole  <- digits
  -- fracDs holds JUST the digits after the dot, not the dot itself, so we
  -- can compute the coefficient and adjusted exponent without rejoining.
  fracDs <- (char '.' *> digits) <|> pure T.empty
  -- pExp returns (digit-count, parsed Int value with sign applied). The cap
  -- check is OUTSIDE the <|> so exceeding it is a hard fail (does not
  -- backtrack into "no exponent"). Lazy evaluation prevents the parsed Int
  -- value from being computed when the cap check would reject anyway.
  expoR  <- (Just <$> pExp) <|> pure Nothing
  case expoR of
    Just (n, _) | n > expCap ->
      pFailStr ("exponent length (" ++ show n ++ ") > " ++ show expCap)
    _ -> pure ()
  pure (whole, fracDs, fmap snd expoR)
  where
    pExp :: Parser (Int, Int)
    pExp = do
      _    <- char 'e' <|> char 'E'
      sign <- (char '-' *> pure (-1)) <|> (char '+' *> pure 1) <|> pure 1
      ds   <- digits
      pure (T.length ds, sign * fromInteger (digitsToInteger ds))

-- | Internal: build a 'Scientific' from 'sciParts' output.
buildScientific :: (Text, Text, Maybe Int) -> Scientific
buildScientific (whole, fracDs, expo) =
    Sci.scientific coeff adjE
  where
    coeff = digitsToInteger (whole <> fracDs)
    adjE  = maybe 0 id expo - T.length fracDs

-- | Raw scientific-number parser (doesn't eat comments). Lenient — accepts
-- integer-shape input as a valid scientific value (@parse sci "42"@ produces
-- @Sci.scientific 42 0@). Returns the literal as a 'Data.Scientific.Scientific'
-- (coefficient × 10^exponent, exact), losslessly preserving the input.
--
-- Use 'sci' (or 'scientific'/'expScientific' from MiniParser.Parser) when
-- you want to parse any numeric form and decide later — e.g. distinguish
-- @42@ from @42.0@ via 'Sci.isInteger', or range-check via
-- 'Sci.toBoundedInteger' before narrowing. For strict-fractional input
-- (Megaparsec's @float@ semantics), use 'fp' instead.
--
-- The exponent length cap is enforced as a hard failure: exceeding it rejects
-- the whole input. An absent or ill-formed exponent prefix (e.g. @3e@,
-- @3eX@) is tolerated — the leading number is consumed and the rest left
-- in the remainder.
--
-- The parser does not consume a leading sign; compose with 'signed' for
-- signed input (matching the convention used by the integer parsers).
sci :: Int -> Parser Scientific
sci = fmap buildScientific . sciParts

-- | Raw floating-point parser (doesn't eat comments). Strict-fractional —
-- the input must contain a @.@ followed by digits, or an exponent
-- (@e@/@E@ optionally signed, followed by digits). Bare integer-shape
-- input fails. This matches Megaparsec's
-- 'Text.Megaparsec.Char.Lexer.float'.
--
-- Use 'fp' (or 'float'/'expFloat' from MiniParser.Parser) when the source
-- language distinguishes integer literals from float literals lexically
-- (@42@ vs @42.0@). For lenient parsing — accept any numeric shape and
-- decide later — use 'sci'/'scientific'.
--
-- Implementation: parses to 'Scientific' via the shared 'sciParts'
-- helper, asserts that at least one of fractional or exponent is present,
-- then narrows via 'Sci.toRealFloat' (IEEE correctly-rounded).
--
-- Type narrowed in 0.5.1.0 from @RealFrac r@ to @RealFloat r@; semantics
-- narrowed to strict-fractional in 0.5.2.0.
fp :: RealFloat r => Int -> Parser r
fp expCap = do
  parts@(_, fracDs, expo) <- sciParts expCap
  case (T.null fracDs, expo) of
    (True, Nothing) -> empty   -- bare integer-shape input — reject
    _otherwise      -> pure ()
  pure (Sci.toRealFloat (buildScientific parts))

-- | Convert a digit-only Text to Integer via left-fold. Always succeeds on
-- input that 'digits' produces (only ASCII '0'..'9'). O(n) time and memory.
digitsToInteger :: Text -> Integer
digitsToInteger = T.foldl' step 0
  where step acc c = acc * 10 + fromIntegral (fromEnum c - fromEnum '0')

-- common digit string parser used by dec, hex and bin
-- digs digTest pmult:
--   digTest:  a (Char -> Bool) test for whether we found a digit
--   pmult:    positional multiplier, 16 for hex, 10 for decimal, 8 for octal, 2 for binary
digs :: Num a => (Char -> Bool) -> a -> Parser a
digs digTest pmult = P $ \(PState pos inp) ->
  let
    ds = T.takeWhile digTest inp
  in
    if T.null ds
    then case T.uncons inp of
      Nothing -> Left [EndOfInput]
      Just (c, _) -> Left [Unexpected' [c]]
    else
      let
        -- Use Data.List.foldl' instead of T.foldl' (not available in MHS)
        val  = foldl' (\acc c -> acc * pmult + fromIntegral (digitToInt c)) 0 (T.unpack ds)
        ps   = PState (advanceText ds pos) (T.drop (T.length ds) inp)
      in
        Right (val, ps)

-- handle whitespace characters: ' ','\n','\r','\t',etc.
-- uses Data.Char.isSpace internally
space :: Parser ()
space = pTakeWhile isSpace *> pure ()

-- look ahead one character, don't ignore whitespace or inline comments
lookAhead :: Parser Char
lookAhead = P $
  \ps@(PState _pos inp) ->
    case T.uncons inp of
      Nothing     -> Left [EndOfInput]
      Just (x, _) -> Right (x, ps)

-- look ahead cnt number of characters, don't ignore whitespace or inline comments
lookAheadMulti :: Int -> Parser Text
lookAheadMulti cnt = P $
  \ps@(PState _pos inp) ->
    let
      taken = T.take cnt inp
    in
      if T.length taken >= cnt
      then Right (taken, ps)
      else Left [EndOfInput]

-- takeUntil family of functions have following contract:
-- ┌────────────────┬───────────────────────────────┬─────────────────────────────┐
-- │     Parser     │         Target found          │      Target not found       │
-- ├────────────────┼───────────────────────────────┼─────────────────────────────┤
-- │ takeUntil      │ Right (before, targetAndRest) │ Right (allInput, "")        │
-- ├────────────────┼───────────────────────────────┼─────────────────────────────┤
-- │ takeUntilStr   │ Right (before, targetAndRest) │ Right (allInput, "")        │
-- ├────────────────┼───────────────────────────────┼─────────────────────────────┤
-- │ takeUntil'     │ Right (before, afterTarget)   │ Left (can't consume target) │
-- ├────────────────┼───────────────────────────────┼─────────────────────────────┤
-- │ takeUntilStr'  │ Right (before, afterTarget)   │ Left (can't consume target) │
-- └────────────────┴───────────────────────────────┴─────────────────────────────┘

-- Read until we see the input character.
-- Don't consume that input character.
takeUntil :: Char -> Parser Text
takeUntil c = pTakeWhile (/= c)

-- Read until we see the input character.
-- DO consume that input character and throw it away.
takeUntil' :: Char -> Parser Text
takeUntil' c = do
  s <- takeUntil c
  _ <- item  -- consume the Char we were looking for
  return s

-- Read until we see the input string.
-- Don't consume the input string.
-- Complexity: O(n) average, O(n*m) worst case, where n = input length and
-- m = search string length. A first-character guard avoids the full isPrefixOf
-- comparison at most positions, giving O(n) in practice for typical inputs.
-- Worst case (e.g., searching for "aab" in "aaaa...aab") is still O(n*m).
-- For guaranteed O(n+m), a KMP implementation could replace the naive scan.
takeUntilStr :: Text -> Parser Text
takeUntilStr srch = P $ \(PState pos inp) ->
  if T.null srch
  then Right (T.empty, PState pos inp)
  else
    let
      srchHead = T.head srch
      go :: Int -> Pos -> Text -> Either [Error] (Text, PState)
      go cnt p remain =
        case T.uncons remain of
          Nothing -> Right (T.take cnt inp, PState p remain)
          Just (c, remain')
            | c == srchHead && srch `T.isPrefixOf` remain ->
                Right (T.take cnt inp, PState p remain)
            | otherwise -> go (cnt + 1) (advanceChar c p) remain'
  in go 0 pos inp

-- Read until we see the input string.
-- Do consume the input string.
takeUntilStr' :: Text -> Parser Text
takeUntilStr' s = do
  pre <- takeUntilStr s
  _ <- string s
  return pre

-- Don't consume EOL chars
takeUntilEOL :: Parser Text
takeUntilEOL = pTakeWhile (\x -> x /= '\n' && x /= '\r')

-- Do consume EOL chars
takeUntilEOL' :: Parser Text
takeUntilEOL' = do
  l <- takeUntilEOL
  _ <- optional $ pTakeWhile (\x -> x == '\n' || x == '\r')
  return l

-- read until EOF
takeAll :: Parser Text
takeAll = P $
  \(PState pos inp) ->
    Right (inp, PState (advanceText inp pos) T.empty)

-- parser that expects EOF.
-- returns () when we reach EOF, throws an error when data
-- remains in the Text stream.
eof :: Parser ()
eof = P $
  \ps@(PState _pos inp) ->
    case T.uncons inp of
      Nothing     -> Right ((), ps)
      Just (x, _) -> Left [ExpectedEndOfFile x]

-- utility parser to throw an error with a custom error string
pFailStr :: String -> Parser a
pFailStr errStr = P (\_ -> Left [CustomError errStr])

-- utility parser to throw one of the standard errors
pFail :: [Error] -> Parser a
pFail errs = P (\_ -> Left errs)

-- take zero or more (always succeeds)
pTakeWhile :: (Char -> Bool) -> Parser Text
pTakeWhile test = P $ \(PState pos inp) ->
  let
    taken = T.takeWhile test inp
    rest  = T.dropWhile test inp
  in
    Right (taken, PState (advanceText taken pos) rest)

-- take 1 or more (fails when no match)
pTakeWhile1 :: (Char -> Bool) -> Parser Text
pTakeWhile1 test = do
  t <- pTakeWhile test
  if T.null t
    then do
      c <- lookAhead -- EndOfInput if empty
      pFail [Unexpected' [c]]
    else
      return t

-- iterate through a list of choices
-- "choice [ p1, p2, ..., pN ]" is the same as "p1 <|> p2 <|> ... <|> pN"
choice :: [Parser a] -> Parser a
choice = foldr (<|>) empty

-- pDiscard consumes and pDiscards whitespace & all types of parsed items
-- from the ordered array of passed in parsers. This is used primarily for
-- removing comments.
-- The list of parsers is an ordered list; they are tried in order, so
-- for example, when parsing Java, you would want to search for API comments
-- (which start with "/**") before parsing inline comments (which start with "/*").
pDiscard :: [Parser ()] -> Parser ()
pDiscard ps = P $ \(PState pos inp) ->
  let
    ws = T.takeWhile isSpace inp
    inp' = T.drop (T.length ws) inp
    pos' = advanceText ws pos
    P tryComment = foldr (<|>) empty ps
  in case tryComment (PState pos' inp') of
       Right ((), st') ->
         let P k = pDiscard ps
         in k st'
       Left _ -> Right ((), PState pos' inp')

-- Parse a block comment that may contain nested occurrences of itself.
-- Takes open and close delimiters; returns content between the outermost pair.
-- Used by languages that support nested comments (Haskell, Swift, Kotlin, etc.).
-- For languages that do NOT nest (C, Java), use takeUntilStr' instead.
nestedBlockComment :: Text -> Text -> Parser Text
nestedBlockComment open close = do
  _ <- string open
  P $ \(PState pos inp) ->
    let
      openLen = T.length open
      closeLen = T.length close
      go :: Int -> Int -> Pos -> Text -> Either [Error] (Text, PState)
      go cnt depth p remain
        | close `T.isPrefixOf` remain =
            let p' = advanceText close p
                remain' = T.drop closeLen remain
             in
               if depth == 1
               then Right (T.take cnt inp, PState p' remain')
               else go (cnt + closeLen) (depth - 1) p' remain'
        | open `T.isPrefixOf` remain =
            let
              p' = advanceText open p
              remain' = T.drop openLen remain
            in
              go (cnt + openLen) (depth + 1) p' remain'
        | otherwise =
          case T.uncons remain of
            Nothing            -> Left [EndOfInput]
            Just (c, remain')  -> go (cnt + 1) depth (advanceChar c p) remain'
    in go 0 1 pos inp

-- Zero or more items separated by sep. Always succeeds (may return []).
sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy p sep = sepBy1 p sep <|> pure []

-- One or more items separated by sep. Fails if no items match.                                   
sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy1 p sep = do
  x <- p
  xs <- many (sep *> p)
  return (x : xs)

errorsToString :: [Error] -> String
errorsToString [] = ""
errorsToString [e] = show e
errorsToString (e:es) = show e ++ " " ++ errorsToString es
