module Service.Main where

import Service.Config (loadConfigFromFile, defaultConfig, Config(..))
import Service.Pipeline (processComparison, processComparisonWithConfig, ComparisonResult(..), ComparisonMetrics(..))
import Service.OutputWriter (OutputResult(..), mkComparisonOutput, mkErrorOutput, writeOutput)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [file1, file2] -> do
      result <- processComparison file1 file2
      handleResult defaultConfig file1 file2 result
    ["--config", configFile, file1, file2] -> do
      configResult <- loadConfigFromFile configFile
      case configResult of
        Left err -> do
          putStrLn $ "Config error: " ++ err
          exitFailure
        Right config -> do
          result <- processComparisonWithConfig config file1 file2
          handleResult config file1 file2 result
    _ -> do
      putStrLn "Usage: etf-comparator <file1.csv> <file2.csv>"
      putStrLn "   or: etf-comparator --config <config-file> <file1.csv> <file2.csv>"
      exitFailure

handleResult :: Config -> FilePath -> FilePath -> ComparisonResult -> IO ()
handleResult config file1 file2 result = do
  -- Write output file for both success and failure cases
  outputResult <- case result of
    ComparisonSuccess metrics unresolved -> do
      compOut <- mkComparisonOutput file1 file2 metrics unresolved
      return $ OutputSuccess compOut
    ComparisonError errInfos -> do
      errOuts <- mapM (\(_, failedFile, allErrors) -> mkErrorOutput failedFile allErrors) errInfos
      return $ OutputFailure errOuts

  -- Write the output file
  writeOutput (outputDirectory config) outputResult

  -- Then handle console output and exit codes
  case result of
    ComparisonSuccess (ComparisonMetrics cosSim jacSim overlap) _ -> do
      putStrLn "ETF comparison completed successfully!"
      putStrLn $ "Cosine Similarity: " ++ show cosSim
      putStrLn $ "Weighted Jaccard Similarity: " ++ show jacSim
      putStrLn $ "Overlap Ratio: " ++ show overlap
      putStrLn $ "Output written to: " ++ outputDirectory config
      exitSuccess
    ComparisonError errInfos -> do
      putStrLn "Comparison error(s):"
      mapM_ (\(firstError, failedFile, allErrors) -> do
        putStrLn $ "  File: " ++ failedFile
        putStrLn $ "  First error: " ++ firstError
        putStrLn $ "  Total errors: " ++ show (length allErrors)
        putStrLn $ "  All errors:"
        mapM_ (\e -> putStrLn $ "    - " ++ show e) allErrors) errInfos
      putStrLn $ "Error details written to: " ++ outputDirectory config
      exitFailure
