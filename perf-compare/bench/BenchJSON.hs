{-# LANGUAGE OverloadedStrings #-}

-- JSON parsing benchmark: MiniParser vs Megaparsec vs Attoparsec
-- Parses into a simple Value ADT (no HashMap/Vector for fairness).

module Main where

import Criterion.Main
import Control.Applicative ((<|>), many)
import Control.DeepSeq (NFData(..))
import Data.Text (Text)
import Data.Void (Void)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified MiniParser.Base as MP
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as MC
import qualified Text.Megaparsec.Char.Lexer as ML
import qualified Data.Attoparsec.Text as A

-- =========================================================================
-- Common result type
-- =========================================================================

data Value
  = Object [(Text, Value)]
  | Array  [Value]
  | String !Text
  | Number !Int
  | Bool   !Bool
  | Null
  deriving (Show, Eq)

instance NFData Value where
  rnf (Object ps) = rnf ps
  rnf (Array vs)  = rnf vs
  rnf (String s)  = rnf s
  rnf (Number n)  = rnf n
  rnf (Bool b)    = rnf b
  rnf Null        = ()

countValues :: Value -> Int
countValues (Object ps) = 1 + sum (map (countValues . snd) ps)
countValues (Array vs)  = 1 + sum (map countValues vs)
countValues _           = 1

-- =========================================================================
-- MiniParser JSON
-- =========================================================================

mpJSON :: MP.Parser Value
mpJSON = mpSpace *> mpValue

mpSpace :: MP.Parser ()
mpSpace = MP.pTakeWhile (\c -> c == ' ' || c == '\n' || c == '\r' || c == '\t') *> pure ()

mpValue :: MP.Parser Value
mpValue = mpObject <|> mpArray <|> mpString <|> mpNumber <|> mpBool <|> mpNull

mpObject :: MP.Parser Value
mpObject = do
  _ <- MP.char '{'; mpSpace
  ps <- mpCommaSep mpPair '}'
  return (Object ps)

mpPair :: MP.Parser (Text, Value)
mpPair = do
  k <- mpJString; mpSpace
  _ <- MP.char ':'; mpSpace
  v <- mpValue; mpSpace
  return (k, v)

mpArray :: MP.Parser Value
mpArray = do
  _ <- MP.char '['; mpSpace
  vs <- mpCommaSep mpValueSp ']'
  return (Array vs)
  where mpValueSp = do v <- mpValue; mpSpace; return v

mpCommaSep :: MP.Parser a -> Char -> MP.Parser [a]
mpCommaSep p end = do
  c <- MP.lookAhead
  if c == end
    then MP.item *> return []
    else mpLoop
  where
    mpLoop = do
      v <- p
      c <- MP.item
      if c == ','
        then do mpSpace; rs <- mpLoop; return (v:rs)
        else return [v]  -- must be end char

mpString :: MP.Parser Value
mpString = String <$> mpJString

mpJString :: MP.Parser Text
mpJString = do
  _ <- MP.char '"'
  s <- MP.pTakeWhile (/= '"')
  _ <- MP.char '"'
  return s

mpNumber :: MP.Parser Value
mpNumber = Number <$> mpSignedInt

mpSignedInt :: MP.Parser Int
mpSignedInt = do
  c <- MP.lookAhead
  if c == '-'
    then do _ <- MP.item; n <- MP.dec; return (negate n)
    else MP.dec

mpBool :: MP.Parser Value
mpBool = (Bool True  <$ MP.string "true")
     <|> (Bool False <$ MP.string "false")

mpNull :: MP.Parser Value
mpNull = Null <$ MP.string "null"

parseJSON_MP :: Text -> Value
parseJSON_MP t =
  case MP.parse mpJSON t of
    Left err -> error ("MiniParser JSON failed: " ++ show err)
    Right (v, _, _) -> v

-- =========================================================================
-- Megaparsec JSON
-- =========================================================================

type MParser = M.Parsec Void Text

mgJSON :: MParser Value
mgJSON = MC.space *> mgValue

mgValue :: MParser Value
mgValue = mgObject <|> mgArray <|> mgString <|> mgNumber <|> mgBool <|> mgNull

mgObject :: MParser Value
mgObject = do
  _ <- MC.char '{'; MC.space
  ps <- mgCommaSep mgPair '}'
  return (Object ps)

mgPair :: MParser (Text, Value)
mgPair = do
  k <- mgJString; MC.space
  _ <- MC.char ':'; MC.space
  v <- mgValue; MC.space
  return (k, v)

mgArray :: MParser Value
mgArray = do
  _ <- MC.char '['; MC.space
  vs <- mgCommaSep mgValueSp ']'
  return (Array vs)
  where mgValueSp = do v <- mgValue; MC.space; return v

mgCommaSep :: MParser a -> Char -> MParser [a]
mgCommaSep p end = do
  w <- M.lookAhead M.anySingle
  if w == end
    then M.anySingle *> return []
    else mgLoop
  where
    mgLoop = do
      v <- p
      c <- M.anySingle
      if c == ','
        then do MC.space; rs <- mgLoop; return (v:rs)
        else return [v]

mgString :: MParser Value
mgString = String <$> mgJString

mgJString :: MParser Text
mgJString = do
  _ <- MC.char '"'
  s <- M.takeWhileP (Just "string char") (/= '"')
  _ <- MC.char '"'
  return s

mgNumber :: MParser Value
mgNumber = Number <$> ML.signed MC.space ML.decimal

mgBool :: MParser Value
mgBool = (Bool True  <$ M.chunk "true")
     <|> (Bool False <$ M.chunk "false")

mgNull :: MParser Value
mgNull = Null <$ M.chunk "null"

parseJSON_MG :: Text -> Value
parseJSON_MG t =
  case M.parse mgJSON "" t of
    Left err -> error (M.errorBundlePretty err)
    Right v -> v

-- =========================================================================
-- Attoparsec JSON
-- =========================================================================

atJSON :: A.Parser Value
atJSON = atSpace *> atValue

atSpace :: A.Parser ()
atSpace = A.skipWhile (\c -> c == ' ' || c == '\n' || c == '\r' || c == '\t')

atValue :: A.Parser Value
atValue = atObject <|> atArray <|> atString <|> atNumber <|> atBool <|> atNull

atObject :: A.Parser Value
atObject = do
  _ <- A.char '{'; atSpace
  ps <- atCommaSep atPair '}'
  return (Object ps)

atPair :: A.Parser (Text, Value)
atPair = do
  k <- atJString; atSpace
  _ <- A.char ':'; atSpace
  v <- atValue; atSpace
  return (k, v)

atArray :: A.Parser Value
atArray = do
  _ <- A.char '['; atSpace
  vs <- atCommaSep atValueSp ']'
  return (Array vs)
  where atValueSp = do v <- atValue; atSpace; return v

atCommaSep :: A.Parser a -> Char -> A.Parser [a]
atCommaSep p end = do
  c <- A.peekChar'
  if c == end
    then A.anyChar *> return []
    else atLoop
  where
    atLoop = do
      v <- p
      c <- A.anyChar
      if c == ','
        then do atSpace; rs <- atLoop; return (v:rs)
        else return [v]

atString :: A.Parser Value
atString = String <$> atJString

atJString :: A.Parser Text
atJString = do
  _ <- A.char '"'
  s <- A.takeWhile (/= '"')
  _ <- A.char '"'
  return s

atNumber :: A.Parser Value
atNumber = Number <$> A.signed A.decimal

atBool :: A.Parser Value
atBool = (Bool True  <$ A.string "true")
     <|> (Bool False <$ A.string "false")

atNull :: A.Parser Value
atNull = Null <$ A.string "null"

parseJSON_AT :: Text -> Value
parseJSON_AT t =
  case A.parseOnly atJSON t of
    Left err -> error ("Attoparsec JSON failed: " ++ err)
    Right v -> v

-- =========================================================================
-- Main
-- =========================================================================

main :: IO ()
main = do
  jsonData <- TIO.readFile "bench-data/json-100.json"

  let mpResult = parseJSON_MP jsonData
      mgResult = parseJSON_MG jsonData
      atResult = parseJSON_AT jsonData
  putStrLn $ "MiniParser:  " ++ show (countValues mpResult) ++ " values"
  putStrLn $ "Megaparsec:  " ++ show (countValues mgResult) ++ " values"
  putStrLn $ "Attoparsec:  " ++ show (countValues atResult) ++ " values"

  defaultMain
    [ bgroup "json-100"
      [ bench "MiniParser"  $ nf parseJSON_MP jsonData
      , bench "Megaparsec"  $ nf parseJSON_MG jsonData
      , bench "Attoparsec"  $ nf parseJSON_AT jsonData
      ]
    ]
