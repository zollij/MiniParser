{-# LANGUAGE OverloadedStrings #-}

-- CSV parsing benchmark: MiniParser vs Megaparsec vs Attoparsec
-- All three parse the same Text input into the same result type.

module Main where

import Criterion.Main
import Control.Applicative ((<|>), many)
import Data.Text (Text)
import Data.Void (Void)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified MiniParser.Base as MP
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as MC
import qualified Data.Attoparsec.Text as A

-- =========================================================================
-- Common result type
-- =========================================================================

type Record = [Field]
type Field  = Text

-- =========================================================================
-- MiniParser CSV
-- =========================================================================

mpCSV :: MP.Parser [Record]
mpCSV = do
  rs <- mpSepEndBy1 mpRecord (MP.char '\n')
  MP.eof
  return rs

mpRecord :: MP.Parser Record
mpRecord = mpSepBy1 mpField (MP.char ',')

mpField :: MP.Parser Field
mpField = mpEscapedField <|> mpUnescapedField

mpEscapedField :: MP.Parser Field
mpEscapedField = do
  _ <- MP.char '"'
  cs <- many (mpNormalChar <|> mpEscapedDQ)
  _ <- MP.char '"'
  return (T.pack cs)
  where
    mpNormalChar = MP.satisfy (/= '"')
    mpEscapedDQ  = MP.char '"' *> MP.char '"'

mpUnescapedField :: MP.Parser Field
mpUnescapedField = MP.pTakeWhile (\c -> c /= ',' && c /= '"' && c /= '\n' && c /= '\r')

mpSepBy1 :: MP.Parser a -> MP.Parser sep -> MP.Parser [a]
mpSepBy1 p sep = do
  x <- p
  xs <- many (sep *> p)
  return (x:xs)

mpSepEndBy1 :: MP.Parser a -> MP.Parser sep -> MP.Parser [a]
mpSepEndBy1 p sep = do
  x <- p
  rs <- (sep *> go) <|> return []
  return (x:rs)
  where
    go = (do x' <- p; rs <- (sep *> go) <|> return []; return (x':rs))
         <|> return []

parseCSV_MP :: Text -> [Record]
parseCSV_MP t =
  case MP.parse mpCSV t of
    Left err -> error ("MiniParser CSV failed: " ++ show err)
    Right (rs, _, _) -> rs

-- =========================================================================
-- Megaparsec CSV
-- =========================================================================

type MParser = M.Parsec Void Text

mgCSV :: MParser [Record]
mgCSV = do
  rs <- M.sepEndBy1 mgRecord MC.eol
  M.eof
  return rs

mgRecord :: MParser Record
mgRecord = do
  M.notFollowedBy M.eof
  M.sepBy1 mgField (MC.char ',') M.<?> "record"

mgField :: MParser Field
mgField = M.label "field" (mgEscapedField <|> mgUnescapedField)

mgEscapedField :: MParser Field
mgEscapedField = do
  _ <- MC.char '"'
  cs <- many (M.anySingleBut '"' <|> ('"' <$ M.chunk "\"\""))
  _ <- MC.char '"'
  return (T.pack cs)

mgUnescapedField :: MParser Field
mgUnescapedField = M.takeWhileP (Just "unescaped char")
  (\c -> c /= ',' && c /= '"' && c /= '\n' && c /= '\r')

parseCSV_MG :: Text -> [Record]
parseCSV_MG t =
  case M.parse mgCSV "" t of
    Left err -> error (M.errorBundlePretty err)
    Right rs -> rs

-- =========================================================================
-- Attoparsec CSV
-- =========================================================================

atCSV :: A.Parser [Record]
atCSV = do
  rs <- atSepEndBy1 atRecord (A.char '\n')
  A.endOfInput
  return rs

atRecord :: A.Parser Record
atRecord = A.sepBy1 atField (A.char ',')

atField :: A.Parser Field
atField = atEscapedField <|> atUnescapedField

atEscapedField :: A.Parser Field
atEscapedField = do
  _ <- A.char '"'
  cs <- many (A.notChar '"' <|> ('"' <$ A.string "\"\""))
  _ <- A.char '"'
  return (T.pack cs)

atUnescapedField :: A.Parser Field
atUnescapedField = A.takeWhile
  (\c -> c /= ',' && c /= '"' && c /= '\n' && c /= '\r')

atSepEndBy1 :: A.Parser a -> A.Parser sep -> A.Parser [a]
atSepEndBy1 p sep = do
  x <- p
  rs <- (sep *> go) <|> return []
  return (x:rs)
  where
    go = (do x' <- p; rs <- (sep *> go) <|> return []; return (x':rs))
         <|> return []

parseCSV_AT :: Text -> [Record]
parseCSV_AT t =
  case A.parseOnly atCSV t of
    Left err -> error ("Attoparsec CSV failed: " ++ err)
    Right rs -> rs

-- =========================================================================
-- Main
-- =========================================================================

main :: IO ()
main = do
  csv100 <- TIO.readFile "bench-data/csv-100.csv"

  -- Sanity check: all three produce the same number of records
  let mpResult = parseCSV_MP csv100
      mgResult = parseCSV_MG csv100
      atResult = parseCSV_AT csv100
  putStrLn $ "MiniParser:  " ++ show (length mpResult) ++ " records, "
           ++ show (sum (map length mpResult)) ++ " fields"
  putStrLn $ "Megaparsec:  " ++ show (length mgResult) ++ " records, "
           ++ show (sum (map length mgResult)) ++ " fields"
  putStrLn $ "Attoparsec:  " ++ show (length atResult) ++ " records, "
           ++ show (sum (map length atResult)) ++ " fields"

  defaultMain
    [ bgroup "csv-100"
      [ bench "MiniParser"  $ nf parseCSV_MP csv100
      , bench "Megaparsec"  $ nf parseCSV_MG csv100
      , bench "Attoparsec"  $ nf parseCSV_AT csv100
      ]
    ]
