module Main where

import DemoComparison (runDemo)

main :: IO ()
main = do
  putStrLn "=== ETF Comparison Demo ==="
  putStrLn ""
  
  putStrLn "Test 1: Correct file first, error file second"
  runDemo "test/config.yaml" "test/input/20260105-CF-correct.csv" "test/input/20260105-CF-multi-errors.csv"
  
  putStrLn "Test 2: Error file first, correct file second"
  runDemo "test/config.yaml" "test/input/20260105-CF-multi-errors.csv" "test/input/20260105-CF-correct.csv"
  
  putStrLn "=== Demo Complete ==="