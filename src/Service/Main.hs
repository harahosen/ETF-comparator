module Service.Main where

import Service.Config (loadConfigFromFile, defaultConfig)
import Service.Pipeline (processComparison, processComparisonWithConfig, ComparisonResult(..), ComparisonMetrics(..))
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [file1, file2] -> do
      result <- processComparison file1 file2
      handleResult result
    ["--config", configFile, file1, file2] -> do
      configResult <- loadConfigFromFile configFile
      case configResult of
        Left err -> do
          putStrLn $ "Config error: " ++ err
          exitFailure
        Right config -> do
          result <- processComparisonWithConfig config file1 file2
          handleResult result
    _ -> do
      putStrLn "Usage: etf-comparator <file1.csv> <file2.csv>"
      putStrLn "   or: etf-comparator --config <config-file> <file1.csv> <file2.csv>"
      exitFailure

handleResult :: ComparisonResult -> IO ()
handleResult result =
  case result of
    ComparisonSuccess (ComparisonMetrics cosSim jacSim overlap) -> do
      putStrLn "ETF comparison completed successfully!"
      putStrLn $ "Cosine Similarity: " ++ show cosSim
      putStrLn $ "Weighted Jaccard Similarity: " ++ show jacSim
      putStrLn $ "Overlap Ratio: " ++ show overlap
      exitSuccess
    ComparisonError err -> do
      putStrLn $ "Comparison error: " ++ err
      exitFailure