{-# LANGUAGE OverloadedStrings #-}

-- MiniParser-only benchmark for comparing GHC vs MHS compilation.
-- No external dependencies beyond base and text.
--
-- Usage:
--   GHC: cabal run bench-miniparser -- [ITERS]
--   MHS: mhs -a~/.mcabal/mhs-0.15.3.0/packages -isrc -ibench -r bench/BenchMiniParser.hs [ITERS]
--
-- ITERS defaults to 1000 for GHC, use a smaller value (e.g. 10) for MHS.

module Main where

import MiniParser.Base
import Control.Applicative ((<|>), many)
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.CPUTime
import System.Environment (getArgs)

-- =========================================================================
-- Timing helper
-- =========================================================================

timeMs :: IO a -> IO (a, Double)
timeMs action = do
  start <- getCPUTime
  result <- action
  end <- getCPUTime
  let ms = fromIntegral (end - start) / 1e9 :: Double
  return (result, ms)

-- Force n iterations of a parse. Uses writeIORef/readIORef to
-- prevent GHC from lifting the pure computation out of the loop.
runN :: Int -> (Text -> Int) -> Text -> IO ()
runN n parser input = do
  ref <- newIORef input
  go ref n
  where
    go _   0 = return ()
    go ref i = do
      inp <- readIORef ref
      let r = parser inp
      r `seq` writeIORef ref inp
      go ref (i - 1)

-- =========================================================================
-- CSV parser
-- =========================================================================

type Record = [Text]

csvParser :: Parser [Record]
csvParser = do
  rs <- sepEndBy1_ csvRecord (char '\n')
  eof
  return rs

csvRecord :: Parser Record
csvRecord = sepBy1_ csvField (char ',')

csvField :: Parser Text
csvField = csvEscaped <|> csvUnescaped

csvEscaped :: Parser Text
csvEscaped = do
  _ <- char '"'
  cs <- many (satisfy (/= '"') <|> (char '"' *> char '"'))
  _ <- char '"'
  return (T.pack cs)

csvUnescaped :: Parser Text
csvUnescaped = pTakeWhile (\c -> c /= ',' && c /= '"' && c /= '\n' && c /= '\r')

-- =========================================================================
-- Log parser
-- =========================================================================

data LogEntry = LogEntry
  !Int !Int !Int  -- year month day
  !Int !Int !Int  -- hour minute second
  !Int !Int !Int !Int  -- IP octets
  !Text  -- product

logParser :: Parser [LogEntry]
logParser = many (logEntry <* char '\n')

logEntry :: Parser LogEntry
logEntry = do
  y  <- nat; _ <- char '-'
  mo <- nat; _ <- char '-'
  d  <- nat; _ <- char ' '
  h  <- nat; _ <- char ':'
  mi <- nat; _ <- char ':'
  s  <- nat; _ <- char ' '
  i1 <- nat; _ <- char '.'
  i2 <- nat; _ <- char '.'
  i3 <- nat; _ <- char '.'
  i4 <- nat; _ <- char ' '
  p  <- pTakeWhile (\c -> c /= '\n' && c /= '\r')
  return (LogEntry y mo d h mi s i1 i2 i3 i4 p)

-- =========================================================================
-- JSON parser
-- =========================================================================

data Value
  = JObject [(Text, Value)]
  | JArray  [Value]
  | JString !Text
  | JNumber !Int
  | JBool   !Bool
  | JNull

jsonParser :: Parser Value
jsonParser = jSpace *> jValue

jSpace :: Parser ()
jSpace = pTakeWhile (\c -> c == ' ' || c == '\n' || c == '\r' || c == '\t') *> pure ()

jValue :: Parser Value
jValue = jObject <|> jArray <|> jString <|> jNumber <|> jBool <|> jNull

jObject :: Parser Value
jObject = do
  _ <- char '{'; jSpace
  ps <- jCommaSep jPair '}'
  return (JObject ps)

jPair :: Parser (Text, Value)
jPair = do
  k <- jStr; jSpace
  _ <- char ':'; jSpace
  v <- jValue; jSpace
  return (k, v)

jArray :: Parser Value
jArray = do
  _ <- char '['; jSpace
  vs <- jCommaSep jValueSp ']'
  return (JArray vs)
  where jValueSp = do v <- jValue; jSpace; return v

jCommaSep :: Parser a -> Char -> Parser [a]
jCommaSep p end = do
  c <- lookAhead
  if c == end
    then item *> return []
    else jLoop
  where
    jLoop = do
      v <- p
      c <- item
      if c == ','
        then do jSpace; rs <- jLoop; return (v:rs)
        else return [v]

jString :: Parser Value
jString = JString <$> jStr

jStr :: Parser Text
jStr = do
  _ <- char '"'
  s <- pTakeWhile (/= '"')
  _ <- char '"'
  return s

jNumber :: Parser Value
jNumber = do
  c <- lookAhead
  if c == '-'
    then do _ <- item; n <- nat; return (JNumber (negate n))
    else JNumber <$> nat

jBool :: Parser Value
jBool = (JBool True  <$ string "true")
    <|> (JBool False <$ string "false")

jNull :: Parser Value
jNull = JNull <$ string "null"

-- =========================================================================
-- Helpers (MHS-compatible sepBy1 / sepEndBy1)
-- =========================================================================

sepBy1_ :: Parser a -> Parser sep -> Parser [a]
sepBy1_ p sep = do
  x <- p
  xs <- many (sep *> p)
  return (x:xs)

sepEndBy1_ :: Parser a -> Parser sep -> Parser [a]
sepEndBy1_ p sep = do
  x <- p
  rs <- (sep *> go) <|> return []
  return (x:rs)
  where
    go = (do x' <- p; rs <- (sep *> go) <|> return []; return (x':rs))
         <|> return []

-- =========================================================================
-- Result counting (forces evaluation without Show on all types)
-- =========================================================================

countRecords :: [Record] -> Int
countRecords rs = length rs

countLogEntries :: [LogEntry] -> Int
countLogEntries es = length es

countValues :: Value -> Int
countValues (JObject ps) = 1 + sum (map (countValues . snd) ps)
countValues (JArray vs)  = 1 + sum (map countValues vs)
countValues _            = 1

-- =========================================================================
-- Parse wrappers
-- =========================================================================

parseCSV :: Text -> Int
parseCSV t = case parse csvParser t of
  Left err -> error ("CSV parse failed: " ++ show err)
  Right (rs, _, _) -> countRecords rs

parseLog :: Text -> Int
parseLog t = case parse logParser t of
  Left err -> error ("Log parse failed: " ++ show err)
  Right (es, _, _) -> countLogEntries es

parseJSON :: Text -> Int
parseJSON t = case parse jsonParser t of
  Left err -> error ("JSON parse failed: " ++ show err)
  Right (v, _, _) -> countValues v

-- =========================================================================
-- Main
-- =========================================================================

main :: IO ()
main = do
  args <- getArgs
  let iters = case args of
        (s:_) -> read s :: Int
        []    -> 1000

  csvData  <- TIO.readFile "bench-data/csv-100.csv"
  logData  <- TIO.readFile "bench-data/log-100.log"
  jsonData <- TIO.readFile "bench-data/json-100.json"

  -- Sanity check
  putStrLn $ "CSV:  " ++ show (parseCSV csvData) ++ " records"
  putStrLn $ "Log:  " ++ show (parseLog logData) ++ " entries"
  putStrLn $ "JSON: " ++ show (parseJSON jsonData) ++ " values"
  putStrLn ""

  putStrLn $ "Running " ++ show iters ++ " iterations each..."
  putStrLn ""

  (_, csvMs) <- timeMs (runN iters parseCSV csvData)
  let csvPer = csvMs / fromIntegral iters
  putStrLn $ "CSV:  " ++ show (round csvMs :: Int) ++ " ms total, "
           ++ showMicro csvPer ++ " per parse"

  (_, logMs) <- timeMs (runN iters parseLog logData)
  let logPer = logMs / fromIntegral iters
  putStrLn $ "Log:  " ++ show (round logMs :: Int) ++ " ms total, "
           ++ showMicro logPer ++ " per parse"

  (_, jsonMs) <- timeMs (runN iters parseJSON jsonData)
  let jsonPer = jsonMs / fromIntegral iters
  putStrLn $ "JSON: " ++ show (round jsonMs :: Int) ++ " ms total, "
           ++ showMicro jsonPer ++ " per parse"

showMicro :: Double -> String
showMicro ms =
  let us = ms * 1000.0
  in show (round us :: Int) ++ " μs"
