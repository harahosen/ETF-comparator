module DemoComparison (runDemo) where

import Service.Config (loadConfigFromFile, Config(..))
import Service.Pipeline (processComparisonWithConfig, ComparisonResult(..), ComparisonMetrics(..))
import Service.OutputWriter (OutputResult(..), mkComparisonOutput, mkErrorOutput, writeOutput)
import System.FilePath ((</>), takeFileName)
import System.Directory (createDirectoryIfMissing, getDirectoryContents, removePathForcibly)

-- Demo function to run comparison and show results
runDemo :: FilePath -> FilePath -> FilePath -> IO ()
runDemo configFile file1 file2 = do
  -- Clean output folder before demo
  cleanupOutputFolder

  configResult <- loadConfigFromFile configFile
  case configResult of
    Left err -> putStrLn $ "Config error: " ++ err
    Right config -> do
      result <- processComparisonWithConfig config file1 file2
      outputResult <- case result of
        ComparisonSuccess metrics unresolved -> do
          compOut <- mkComparisonOutput file1 file2 metrics unresolved
          return $ OutputSuccess compOut
        ComparisonError errInfos -> do
          errOuts <- mapM (\(_, failedFile, allErrors) -> mkErrorOutput failedFile allErrors) errInfos
          return $ OutputFailure errOuts

      writeOutput (outputDirectory config) outputResult

      putStrLn $ "=== Comparison: " ++ takeFileName file1 ++ " vs " ++ takeFileName file2 ++ " ==="
      case result of
        ComparisonSuccess (ComparisonMetrics cosSim jacSim overlap) _ -> do
          putStrLn "SUCCESS:"
          putStrLn $ "  Cosine Similarity: " ++ show cosSim
          putStrLn $ "  Weighted Jaccard Similarity: " ++ show jacSim
          putStrLn $ "  Overlap Ratio: " ++ show overlap
        ComparisonError errInfos -> do
          putStrLn "ERROR:"
          mapM_ (\(firstError, failedFile, allErrors) -> do
            putStrLn $ "  Failed file: " ++ takeFileName failedFile
            putStrLn $ "  First error: " ++ firstError
            putStrLn $ "  Total errors: " ++ show (length allErrors)
            putStrLn $ "  All errors:"
            mapM_ (\e -> putStrLn $ "    - " ++ show e) allErrors) errInfos
      putStrLn $ "Output written to: " ++ outputDirectory config
      putStrLn ""

-- Helper function to clean output folder
cleanupOutputFolder :: IO ()
cleanupOutputFolder = do
  let outputDir = "test" </> "output"
  createDirectoryIfMissing True outputDir
  files <- getDirectoryContents outputDir
  let filesToRemove = filter (not . (`elem` [".", ".."])) files
  mapM_ (removePathForcibly . (outputDir </>)) filesToRemove
