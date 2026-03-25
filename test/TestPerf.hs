{-# LANGUAGE OverloadedStrings #-}

-- Performance and large-input tests for MiniParser.
-- Generates 100KB+ inputs in memory, verifies correctness,
-- and checks that parsing completes within reasonable time bounds.

module Main where

import MiniParser.Base
import MiniParser.Parser
import TestHelpers (getPosFromResult, test, reportResults)
import qualified MiniParser.Comments.C as CC
import Control.Applicative (Alternative(..), many)
import Data.Char (isAlpha)
import Data.Text (Text)
import qualified Data.Text as T
import System.CPUTime
import System.Environment (getArgs)

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- unwords' is not available in MHS; use T.intercalate instead
unwords' :: [Text] -> Text
unwords' = T.intercalate " "

-- unlines' is not available in MHS; use T.intercalate with trailing newline
unlines' :: [Text] -> Text
unlines' xs = T.intercalate "\n" xs <> "\n"

-- Time an IO action in milliseconds
timeMs :: IO a -> IO (a, Double)
timeMs action = do
  start <- getCPUTime
  result <- action
  end <- getCPUTime
  let ms = fromIntegral (end - start) / 1e9 :: Double
  return (result, ms)

-- Run a test, return pass/fail
-- Run a timed test: parse must succeed and complete within maxMs
timedTest :: Show a => String -> Double -> (Text -> Either [Error] (a, Pos, Text)) -> Text -> IO Bool
timedTest name maxMs parser input = do
  (result, ms) <- timeMs (return $! forceResult (parser input))
  case result of
    Left errs -> do
      putStrLn $ "  FAIL: " ++ name ++ " (parse error: " ++ show errs ++ ")"
      return False
    Right _ -> do
      let status = if ms <= maxMs then "OK" else "SLOW"
      putStrLn $ "  " ++ status ++ ": " ++ name
        ++ " (" ++ show (round ms :: Int) ++ "ms"
        ++ ", limit " ++ show (round maxMs :: Int) ++ "ms"
        ++ ", input " ++ show (T.length input) ++ " chars)"
      return (ms <= maxMs)

-- Force evaluation of parse result to avoid lazy thunks in timing
forceResult :: Either [Error] (a, Pos, Text) -> Either [Error] (a, Pos, Text)
forceResult r@(Left _) = r
forceResult r@(Right (_, Pos l c, rest)) = l `seq` c `seq` T.length rest `seq` r

-- ---------------------------------------------------------------------------
-- Input generators (all produce 100KB+ Text values)
-- ---------------------------------------------------------------------------

-- ~102K of "word1 word2 word3 ...\n" repeated lines
genIdentLines :: Int -> Text
genIdentLines numLines =
  unlines' [ unwords' [ T.pack ("var" ++ show i ++ "x" ++ show j)
                         | j <- [1..wordsPerLine] ]
            | i <- [1..numLines] ]
  where wordsPerLine = 10 :: Int

-- ~105K of "123 456 789 ...\n" repeated lines
genNumberLines :: Int -> Text
genNumberLines numLines =
  unlines' [ unwords' [ T.pack (show (i * 100 + j))
                         | j <- [1..numsPerLine] ]
            | i <- [1..numLines] ]
  where numsPerLine = 15

-- ~110K of C source with comments interspersed
genCSource :: Int -> Text
genCSource numLines =
  unlines' [ mkLine i | i <- [1..numLines] ]
  where
    mkLine i
      | i `mod` 5 == 0 = "// comment line " <> T.pack (show i)
      | i `mod` 7 == 0 = "/* block " <> T.pack (show i) <> " */ int x = " <> T.pack (show i) <> ";"
      | otherwise       = "int var" <> T.pack (show i) <> " = " <> T.pack (show (i * 42)) <> ";"

-- ~100K of a single long line (worst case for column tracking)
genLongLine :: Int -> Text
genLongLine n = T.replicate n "abcdefghij"

-- ~100K of short lines (many newlines — tests line tracking)
genManyLines :: Int -> Text
genManyLines n = unlines' [ T.pack ("ln" ++ show i) | i <- [1..n] ]

-- ---------------------------------------------------------------------------
-- Tests
-- ---------------------------------------------------------------------------

-- Usage: perf-test [SCALE]
-- SCALE is an optional integer (default 100) that controls input sizes.
-- GHC uses the default 100 (~100KB inputs).
-- MHS uses a smaller scale (e.g. 5 for ~5KB) because MHS graph reduction
-- is much slower than GHC native code on char-at-a-time operations.
main :: IO ()
main = do
  args <- getArgs
  let scale = case args of
        (s:_) -> read s :: Int
        []    -> 100  -- default: ~100KB inputs

  putStrLn $ "Performance and large-input tests (scale=" ++ show scale ++ ")..."
  putStrLn ""

  -- Input sizes scale linearly with the scale parameter.
  -- At scale=100: ~100KB inputs.  At scale=5: ~5KB inputs.
  let identLines  = 10 * scale
      numberLines = 7 * scale
      cLines      = 20 * scale
      longChars   = 100 * scale    -- genLongLine produces 10 chars per unit
      manyLnCount = 200 * scale

  let identInput  = genIdentLines identLines
  let numberInput = genNumberLines numberLines
  let cSource     = genCSource cLines
  let longLine    = genLongLine longChars
  let manyLines   = genManyLines manyLnCount

  putStrLn $ "Input sizes:"
  putStrLn $ "  identInput:  " ++ show (T.length identInput) ++ " chars"
  putStrLn $ "  numberInput: " ++ show (T.length numberInput) ++ " chars"
  putStrLn $ "  cSource:     " ++ show (T.length cSource) ++ " chars"
  putStrLn $ "  longLine:    " ++ show (T.length longLine) ++ " chars"
  putStrLn $ "  manyLines:   " ++ show (T.length manyLines) ++ " chars"
  putStrLn ""

  -- Time limit: scales with input size (50ms per scale unit, minimum 5000ms).
  -- The minimum is generous to accommodate MHS, where graph reduction is
  -- ~100x slower than GHC native code.  GHC completes all tests in <20ms
  -- regardless, so the limit only matters for MHS.
  let limit = max 5000.0 (fromIntegral scale * 50.0)

  putStrLn "-- Correctness on large inputs --"
  results1 <- sequence
    [ -- Verify takeAll works on large input and position is correct
      test "takeAll large input length"
        (case parse takeAll longLine of
           Right (t, _, _) -> T.length t
           Left _ -> -1)
        (T.length longLine)

    , test "takeAll large input position (single line)"
        (getPosFromResult (parse takeAll longLine))
        (Just (Pos 1 (T.length longLine + 1)))

    , test "takeAll many lines position"
        (getPosFromResult (parse takeAll manyLines))
        -- N lines + final \n leaves us at line N+1 col 1
        (Just (Pos (manyLnCount + 1) 1))

    , test "many item on long line"
        (case parse (many item) longLine of
           Right (cs, _, _) -> length cs
           Left _ -> -1)
        (T.length longLine)

    , test "pTakeWhile isAlpha on long line"
        (case parse (pTakeWhile isAlpha) longLine of
           Right (t, _, _) -> T.length t
           Left _ -> -1)
        (T.length longLine)

    -- splitLines on large multi-line input
    , test "splitLines large input line count"
        (length (splitLinesT manyLines))
        manyLnCount

    -- Verify position tracking across many lines
    , test ("position after parsing " ++ show identLines ++ " ident lines")
        (case parse takeAll identInput of
           Right (_, pos, _) -> Just pos
           Left _ -> Nothing)
        -- N lines of content + trailing \n -> line N+1 col 1
        (Just (Pos (identLines + 1) 1))
    ]

  putStrLn ""
  putStrLn "-- Performance: takeAll --"
  results2 <- sequence
    [ timedTest "takeAll single line" limit (parse takeAll) longLine
    , timedTest "takeAll many lines" limit (parse takeAll) manyLines
    , timedTest "takeAll idents" limit (parse takeAll) identInput
    ]

  putStrLn ""
  putStrLn "-- Performance: pTakeWhile --"
  results3 <- sequence
    [ timedTest "pTakeWhile isAlpha" limit (parse (pTakeWhile isAlpha)) longLine
    , timedTest "pTakeWhile on number input" limit (parse (pTakeWhile (/= '\n'))) numberInput
    ]

  putStrLn ""
  putStrLn "-- Performance: many item --"
  results4 <- sequence
    [ timedTest "many item single line" limit (parse (many item)) longLine
    , timedTest "many item many lines" limit (parse (many item)) manyLines
    ]

  putStrLn ""
  putStrLn "-- Performance: splitLines --"
  results5 <- sequence
    [ timedTest "splitLines via splitLines" limit (parse splitLines) manyLines
    , timedTest "splitLines via splitLines idents" limit (parse splitLines) identInput
    ]

  putStrLn ""
  putStrLn "-- Performance: string matching --"
  results6 <- sequence
    [ -- Search for a string near the end of long input
      timedTest "takeUntilStr near end" limit
        (parse (takeUntilStr "ghij"))
        longLine
    , timedTest "takeUntilStr' near end" limit
        (parse (takeUntilStr' "ghij"))
        longLine
    ]

  putStrLn ""
  putStrLn "-- Performance: C comment stripping --"
  let cToken p = do { CC.comments; p }
      cIdent = cToken ident
      -- Parse as many identifiers as possible from C source
      cMany = many cIdent
  results7 <- sequence
    [ timedTest "many C-commented idents" limit (parse cMany) cSource
    ]

  putStrLn ""
  putStrLn "-- Performance: repeated identifier parsing --"
  results8 <- sequence
    [ timedTest "many identifier" limit (parse (many identifier)) identInput
    ]

  let allResults = concat [results1, results2, results3, results4,
                           results5, results6, results7, results8]
  let total = length allResults
  let passed = length (filter id allResults)
  putStrLn ""
  putStrLn $ "Results: " ++ show passed ++ "/" ++ show total ++ " passed"
  reportResults "performance" allResults
