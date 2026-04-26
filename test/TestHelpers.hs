{-# LANGUAGE OverloadedStrings #-}

-- TestHelpers: shared utilities and a colored, hierarchical test runner.
--
-- Provides a tasty-like output style without adding a tasty dependency.
-- Group/test names are printed indented, with a green "OK" or red "FAIL"
-- right-aligned to the column. Color is auto-detected (NO_COLOR env var,
-- or non-TTY stdout, disables it).
module TestHelpers (
  -- result inspection
  stripPos, getPosFromResult,
  -- legacy IO-list test helpers (used by examples/Test*.hs and TestPerf.hs)
  test, reportResults,
  -- HUnit tree runner
  runHUnit,
  -- QuickCheck wrapper
  runQC,
  -- output primitives
  printResult, printResultLine, suiteHeader, sectionHeader, summaryLine,
  -- colors (in case test files want to colorize their own output)
  green, red, yellow, bold
) where

import MiniParser.Base (Pos)
import Data.Text (Text)
import System.Exit (exitFailure)
import qualified Control.Exception as E
import System.IO (stdout, hIsTerminalDevice)
import System.IO.Unsafe (unsafePerformIO)
import System.Environment (lookupEnv)
import Test.HUnit (Test(..), Assertion)
import Test.QuickCheck (Testable, quickCheckWithResult, stdArgs, Args(..),
                        Result(..))
import Data.Time.Clock (NominalDiffTime)

-- ---------------------------------------------------------------------------
-- Result inspection (existing helpers)
-- ---------------------------------------------------------------------------

stripPos :: Either e (a, Pos, Text) -> Either e (a, Text)
stripPos (Left e) = Left e
stripPos (Right (a, _, rest)) = Right (a, rest)

getPosFromResult :: Either e (a, Pos, Text) -> Maybe Pos
getPosFromResult (Right (_, pos, _)) = Just pos
getPosFromResult (Left _) = Nothing

-- ---------------------------------------------------------------------------
-- Color / TTY detection
-- ---------------------------------------------------------------------------

-- Resolved once at startup. Disables color when NO_COLOR is set or stdout
-- is not a terminal. hIsTerminalDevice can fail on some runtimes (notably
-- MHS), so we wrap it and fall back to a TERM-based check.
{-# NOINLINE colorEnabled #-}
colorEnabled :: Bool
colorEnabled = unsafePerformIO $ do
  noColor <- lookupEnv "NO_COLOR"
  case noColor of
    Just _ -> pure False
    Nothing -> do
      r <- E.try (hIsTerminalDevice stdout) :: IO (Either E.SomeException Bool)
      case r of
        Right b -> pure b
        Left _  -> do
          t <- lookupEnv "TERM"
          pure $ case t of
            Just s | s /= "" && s /= "dumb" -> True
            _ -> False

ansi :: String -> String -> String
ansi code s
  | colorEnabled = "\ESC[" ++ code ++ "m" ++ s ++ "\ESC[0m"
  | otherwise    = s

green, red, yellow, bold :: String -> String
green  = ansi "32"
red    = ansi "31"
yellow = ansi "33"
bold   = ansi "1"

-- ---------------------------------------------------------------------------
-- Output primitives
-- ---------------------------------------------------------------------------

-- Right-align "OK" / "FAIL" / annotation to this column (1-indexed).
nameColumn :: Int
nameColumn = 76

-- printResult indent name status
--   indent: number of 2-space indents (0 = no indent)
--   name:   test name (no trailing colon)
--   status: rendered status string ("OK" / "FAIL" / "GAVE UP")
printResultRaw :: Int -> String -> String -> IO ()
printResultRaw indent name status = do
  let prefix = replicate (indent * 2) ' ' ++ name ++ ": "
      pad = max 1 (nameColumn - length prefix - statusLen status)
  putStrLn $ prefix ++ replicate pad ' ' ++ status
  where
    -- Length of status excluding ANSI escapes (heuristic).
    statusLen s = length (stripAnsi s)
    stripAnsi []                 = []
    stripAnsi ('\ESC':'[':rest)  = stripAnsi (drop 1 (dropWhile (/= 'm') rest))
    stripAnsi (c:cs)             = c : stripAnsi cs

-- Print a pass/fail line (one level of indent).
printResult :: String -> Bool -> IO ()
printResult name passed =
  printResultRaw 1 name (if passed then green "OK" else red "FAIL")

-- Print a result line with a custom status (e.g. for perf-test which wants
-- "OK (4ms, limit 5000ms, input 89930 chars)" trailing the right-aligned
-- label). One level of indent.
printResultLine :: String -> String -> IO ()
printResultLine = printResultRaw 1

-- Print bold suite header followed by a blank line.
suiteHeader :: String -> IO ()
suiteHeader name = do
  putStrLn ""
  putStrLn (bold name)

-- Print bold section header (one level of indent).
sectionHeader :: String -> IO ()
sectionHeader name = putStrLn $ "  " ++ bold name

-- Print final summary: "All N tests passed (X.YYYs)" / "K of N tests failed".
-- Time is always rendered as fixed-point (e.g. "0.013s"), never scientific.
summaryLine :: Int -> Int -> NominalDiffTime -> IO ()
summaryLine passed failed elapsed = do
  putStrLn ""
  let total = passed + failed
      secs  = showSecs (realToFrac elapsed :: Double)
  if failed == 0
    then putStrLn $ green ("All " ++ show total ++ " tests passed") ++
                    " (" ++ secs ++ "s)"
    else putStrLn $ red (show failed ++ " of " ++ show total ++ " tests failed") ++
                    " (" ++ secs ++ "s)"

-- Fixed-point seconds formatter: "0.013" not "1.3e-2".
showSecs :: Double -> String
showSecs x
  | x < 0 = '-' : showSecs (negate x)
  | otherwise =
      let totalMs = round (x * 1000) :: Int
          (whole, ms) = totalMs `divMod` 1000
          msStr = let s = show ms in replicate (3 - length s) '0' ++ s
      in show whole ++ "." ++ msStr

-- ---------------------------------------------------------------------------
-- Legacy helpers (kept for examples/Test*.hs and TestPerf.hs)
-- ---------------------------------------------------------------------------

test :: (Eq a, Show a) => String -> a -> a -> IO Bool
test name actual expected
  | actual == expected = do
      printResult name True
      pure True
  | otherwise = do
      printResult name False
      putStrLn $ "    expected: " ++ show expected
      putStrLn $ "    actual:   " ++ show actual
      pure False

reportResults :: String -> [Bool] -> IO ()
reportResults suiteName results = do
  let failed = length (filter not results)
      total  = length results
  putStrLn ""
  if failed == 0
    then putStrLn $ green ("All " ++ show total ++ " " ++ suiteName ++ " tests passed")
    else do
      putStrLn $ red (show failed ++ " of " ++ show total ++ " " ++ suiteName ++ " tests failed")
      exitFailure

-- ---------------------------------------------------------------------------
-- HUnit tree runner: walk a Test tree, print each TestCase result, recurse
-- into nested TestList groups.
-- ---------------------------------------------------------------------------

runAssertion :: Assertion -> IO (Either String ())
runAssertion a = do
  r <- E.try a :: IO (Either E.SomeException ())
  pure $ case r of
    Right () -> Right ()
    Left e   -> Left (show e)

-- Walks the Test tree at a given indentation level.
-- Returns (passed, failed).
runHUnitAt :: Int -> Test -> IO (Int, Int)
runHUnitAt indent t = case t of
  TestCase a -> runOne indent "<unnamed>" a
  TestLabel name (TestCase a) -> runOne indent name a
  TestLabel name (TestList ts) -> do
    putStrLn $ replicate (indent * 2) ' ' ++ bold name
    foldChildren (indent + 1) ts
  TestLabel name inner -> do
    putStrLn $ replicate (indent * 2) ' ' ++ bold name
    runHUnitAt (indent + 1) inner
  TestList ts -> foldChildren indent ts
  where
    runOne i name a = do
      r <- runAssertion a
      case r of
        Right () -> do
          printResultRaw (i + 1) name (green "OK")
          pure (1, 0)
        Left msg -> do
          printResultRaw (i + 1) name (red "FAIL")
          mapM_ (putStrLn . ("      " ++)) (lines msg)
          pure (0, 1)

    foldChildren i = go (0, 0)
      where
        go acc [] = pure acc
        go (p, f) (x:xs) = do
          (p', f') <- runHUnitAt i x
          go (p + p', f + f') xs

-- Public entry: run a Test tree, print suite header, totals, and exit on
-- failure. Returns (passed, failed) for the caller to combine with other
-- runners (e.g. QuickCheck) before computing the final summary.
runHUnit :: Test -> IO (Int, Int)
runHUnit = runHUnitAt 0

-- ---------------------------------------------------------------------------
-- QuickCheck wrapper: silent run, single colored result line.
-- ---------------------------------------------------------------------------

-- runQC name prop  →  prints "    name:  ...  OK (100 tests)" or FAIL.
-- Indented two levels: callers typically nest QC properties inside a
-- 'sectionHeader' (which prints at indent 1).
-- Returns True iff the result is Success or GaveUp (treats GaveUp as
-- non-fatal because some pre-existing properties have rare preconditions).
runQC :: Testable prop => String -> prop -> IO Bool
runQC name prop = do
  result <- quickCheckWithResult stdArgs { chatty = False } prop
  case result of
    Success { numTests = n } -> do
      printResultRaw 2 name (green "OK" ++ " (" ++ show n ++ " tests)")
      pure True
    GaveUp { numTests = n, numDiscarded = d } -> do
      printResultRaw 2 name (yellow "GAVE UP" ++ " (" ++ show n ++ " passed, " ++ show d ++ " discarded)")
      pure True
    Failure { output = o } -> do
      printResultRaw 2 name (red "FAIL")
      mapM_ (putStrLn . ("      " ++)) (lines o)
      pure False
    NoExpectedFailure { numTests = n } -> do
      printResultRaw 2 name (red "FAIL" ++ " (no expected failure, " ++ show n ++ " tests)")
      pure False
