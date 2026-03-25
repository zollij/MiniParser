module TestHelpers (stripPos, getPosFromResult, test, reportResults) where

import MiniParser.Base (Pos)
import Data.Text (Text)
import System.Exit (exitFailure)

-- Strip position from parse result for comparison
stripPos :: Either e (a, Pos, Text) -> Either e (a, Text)
stripPos (Left e) = Left e
stripPos (Right (a, _, rest)) = Right (a, rest)

-- Extract position from successful parse
getPosFromResult :: Either e (a, Pos, Text) -> Maybe Pos
getPosFromResult (Right (_, pos, _)) = Just pos
getPosFromResult (Left _) = Nothing

-- Run a test, return pass/fail
test :: (Eq a, Show a) => String -> a -> a -> IO Bool
test name actual expected
  | actual == expected = do putStrLn $ "  OK: " ++ name; return True
  | otherwise = do
      putStrLn $ "  FAIL: " ++ name
      putStrLn $ "    expected: " ++ show expected
      putStrLn $ "    actual:   " ++ show actual
      return False

-- Report test results and exit with appropriate code
reportResults :: String -> [Bool] -> IO ()
reportResults suiteName results = do
  let failures = length (filter not results)
  if failures == 0
    then putStrLn $ "All " ++ suiteName ++ " tests passed!"
    else do
      putStrLn $ show failures ++ " " ++ suiteName ++ " test(s) failed!"
      exitFailure
