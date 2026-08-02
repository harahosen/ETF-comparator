module Main where

import Service.Config (loadConfigFromFile, defaultConfig)
import Service.Pipeline (processETF, processETFWithConfig, ProcessResult(..))
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filePath] -> do
      result <- processETF filePath
      handleResult result
    ["--config", configFile, filePath] -> do
      configResult <- loadConfigFromFile configFile
      case configResult of
        Left err -> do
          putStrLn $ "Config error: " ++ err
          exitFailure
        Right config -> do
          result <- processETFWithConfig config filePath
          handleResult result
    _ -> do
      putStrLn "Usage: etf-comparator <file.csv>"
      putStrLn "   or: etf-comparator --config <config-file> <file.csv>"
      exitFailure

handleResult :: ProcessResult -> IO ()
handleResult result =
  case result of
    ProcessSuccess normalizedEtf -> do
      putStrLn "ETF processed successfully!"
      -- Could add output saving here
      exitSuccess
    ProcessValidationError errs -> do
      putStrLn $ "Validation errors: " ++ show errs
      exitFailure
    ProcessNormalizationError err -> do
      putStrLn $ "Normalization error: " ++ show err
      exitFailure
    ProcessMappingError err -> do
      putStrLn $ "Mapping error: " ++ err
      exitFailure
    ProcessUnresolvedAssetIds ids -> do
      putStrLn $ "Unresolved asset IDs: " ++ show ids
      exitFailure
    ProcessUnresolvedFundId fundId -> do
      putStrLn $ "Unresolved fund ID: " ++ show fundId
      exitFailure