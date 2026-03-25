{-# LANGUAGE OverloadedStrings #-}

-- Log file parsing benchmark: MiniParser vs Megaparsec vs Attoparsec
-- Format: "2013-06-29 11:16:23 124.67.34.60 keyboard\n"

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
-- Common result types
-- =========================================================================

data LogEntry = LogEntry
  { leYear, leMonth, leDay     :: !Int
  , leHour, leMinute, leSecond :: !Int
  , leIP1, leIP2, leIP3, leIP4 :: !Int
  , leProduct                   :: !Text
  } deriving (Show, Eq)

instance NFData LogEntry where
  rnf (LogEntry y mo d h mi s i1 i2 i3 i4 p) =
    y `seq` mo `seq` d `seq` h `seq` mi `seq` s `seq`
    i1 `seq` i2 `seq` i3 `seq` i4 `seq` p `seq` ()

-- =========================================================================
-- MiniParser Log
-- =========================================================================

mpLog :: MP.Parser [LogEntry]
mpLog = many (mpEntry <* MP.char '\n')

mpEntry :: MP.Parser LogEntry
mpEntry = do
  y  <- MP.nat; _ <- MP.char '-'
  mo <- MP.nat; _ <- MP.char '-'
  d  <- MP.nat; _ <- MP.char ' '
  h  <- MP.nat; _ <- MP.char ':'
  mi <- MP.nat; _ <- MP.char ':'
  s  <- MP.nat; _ <- MP.char ' '
  i1 <- MP.nat; _ <- MP.char '.'
  i2 <- MP.nat; _ <- MP.char '.'
  i3 <- MP.nat; _ <- MP.char '.'
  i4 <- MP.nat; _ <- MP.char ' '
  p  <- MP.pTakeWhile (\c -> c /= '\n' && c /= '\r')
  return (LogEntry y mo d h mi s i1 i2 i3 i4 p)

parseLog_MP :: Text -> [LogEntry]
parseLog_MP t =
  case MP.parse mpLog t of
    Left err -> error ("MiniParser Log failed: " ++ show err)
    Right (rs, _, _) -> rs

-- =========================================================================
-- Megaparsec Log
-- =========================================================================

type MParser = M.Parsec Void Text

mgLog :: MParser [LogEntry]
mgLog = many (mgEntry <* MC.eol)

mgEntry :: MParser LogEntry
mgEntry = do
  y  <- ML.decimal; _ <- MC.char '-'
  mo <- ML.decimal; _ <- MC.char '-'
  d  <- ML.decimal; _ <- MC.char ' '
  h  <- ML.decimal; _ <- MC.char ':'
  mi <- ML.decimal; _ <- MC.char ':'
  s  <- ML.decimal; _ <- MC.char ' '
  i1 <- ML.decimal; _ <- MC.char '.'
  i2 <- ML.decimal; _ <- MC.char '.'
  i3 <- ML.decimal; _ <- MC.char '.'
  i4 <- ML.decimal; _ <- MC.char ' '
  p  <- M.takeWhileP Nothing (\c -> c /= '\n' && c /= '\r')
  return (LogEntry y mo d h mi s i1 i2 i3 i4 p)

parseLog_MG :: Text -> [LogEntry]
parseLog_MG t =
  case M.parse mgLog "" t of
    Left err -> error (M.errorBundlePretty err)
    Right rs -> rs

-- =========================================================================
-- Attoparsec Log
-- =========================================================================

atLog :: A.Parser [LogEntry]
atLog = many (atEntry <* A.char '\n')

atEntry :: A.Parser LogEntry
atEntry = do
  y  <- A.decimal; _ <- A.char '-'
  mo <- A.decimal; _ <- A.char '-'
  d  <- A.decimal; _ <- A.char ' '
  h  <- A.decimal; _ <- A.char ':'
  mi <- A.decimal; _ <- A.char ':'
  s  <- A.decimal; _ <- A.char ' '
  i1 <- A.decimal; _ <- A.char '.'
  i2 <- A.decimal; _ <- A.char '.'
  i3 <- A.decimal; _ <- A.char '.'
  i4 <- A.decimal; _ <- A.char ' '
  p  <- A.takeWhile (\c -> c /= '\n' && c /= '\r')
  return (LogEntry y mo d h mi s i1 i2 i3 i4 p)

parseLog_AT :: Text -> [LogEntry]
parseLog_AT t =
  case A.parseOnly atLog t of
    Left err -> error ("Attoparsec Log failed: " ++ err)
    Right rs -> rs

-- =========================================================================
-- Main
-- =========================================================================

main :: IO ()
main = do
  logData <- TIO.readFile "bench-data/log-100.log"

  let mpResult = parseLog_MP logData
      mgResult = parseLog_MG logData
      atResult = parseLog_AT logData
  putStrLn $ "MiniParser:  " ++ show (length mpResult) ++ " entries"
  putStrLn $ "Megaparsec:  " ++ show (length mgResult) ++ " entries"
  putStrLn $ "Attoparsec:  " ++ show (length atResult) ++ " entries"

  defaultMain
    [ bgroup "log-100"
      [ bench "MiniParser"  $ nf parseLog_MP logData
      , bench "Megaparsec"  $ nf parseLog_MG logData
      , bench "Attoparsec"  $ nf parseLog_AT logData
      ]
    ]
