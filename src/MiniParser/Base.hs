{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

-- Base is split from Parser to break a circular dependency:
--   Comments.hs needs the Parser type (to define comments :: Parser ())
--   Parser.hs needs Comments.hs (to use comments inside token, identifier, etc.)
-- Base holds the types and primitives that both modules need.
module MiniParser.Base where

import Control.Applicative
import Data.Char
import Data.List (foldl')
import Data.Text (Text)
import qualified Data.Text as T

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

ident :: Parser Text
ident = P $ \(PState pos inp) ->
  case T.uncons inp of  -- get first Char
    Nothing -> Left [EndOfInput]
    Just (c, rest)
      | isLower c ->  -- make sure first Char is lower case
          let
            more = T.takeWhile isAlphaNum rest
            whole = T.cons c more
          in Right (whole, PState (advanceText whole pos) (T.drop (T.length more) rest))
      | otherwise -> Left [Unexpected' [c]]

nat :: Parser Int
nat = P $ \(PState pos inp) ->
  let
    digs = T.takeWhile isDigit inp
  in
    if T.null digs
    then case T.uncons inp of
      Nothing -> Left [EndOfInput]
      Just (c, _) -> Left [Unexpected' [c]]
    else
      let
        -- Use Data.List.foldl' instead of T.foldl' (not available in MHS)
        val  = foldl' (\acc c -> acc * 10 + digitToInt c) 0 (T.unpack digs)
        ps   = PState (advanceText digs pos) (T.drop (T.length digs) inp)
      in
        Right (val, ps)

int :: Parser Int
int = do
  _ <- char '-'
  n <- nat
  return (-n)
  <|> nat

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
-- │ takeUntil     │ Right (before, targetAndRest) │ Right (allInput, "")        │
-- ├────────────────┼───────────────────────────────┼─────────────────────────────┤
-- │ takeUntilStr  │ Right (before, targetAndRest) │ Right (allInput, "")        │
-- ├────────────────┼───────────────────────────────┼─────────────────────────────┤
-- │ takeUntil'    │ Right (before, afterTarget)   │ Left (can't consume target) │
-- ├────────────────┼───────────────────────────────┼─────────────────────────────┤
-- │ takeUntilStr' │ Right (before, afterTarget)   │ Left (can't consume target) │
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

-- Parse an identifier with additional allowed special characters.
-- Like 'ident' but also permits the given characters in the identifier.
identWith :: [Char] -> Parser Text
identWith spec = do
  first <- do choice [letter, special]  -- order matters
  remain <- pTakeWhile (\x -> isAlphaNum x || isSpecial x)
  return $ T.cons first remain
  where
    isSpecial :: Char -> Bool
    isSpecial c = c `elem` spec
    special :: Parser Char
    special = do
      c <- item
      if isSpecial c
        then return c
        else pFailStr "identWith.special"

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
