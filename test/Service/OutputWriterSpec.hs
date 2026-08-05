module Service.OutputWriterSpec (spec) where

import Test.Hspec
import Service.OutputWriter
import Service.Pipeline (ComparisonMetrics(..))
import Domain.Errors (PipelineError(..))
import System.FilePath ((</>))
import System.Directory (createDirectoryIfMissing, doesFileExist, getDirectoryContents, removePathForcibly)
import Data.List (isInfixOf)


spec :: Spec
spec = do
  before_ cleanupOutputFolder $ do
    describe "Service.OutputWriter" $ do
      describe "mkComparisonOutput" $ do
        it "creates comparison output with timestamp" $ do
          let metrics = ComparisonMetrics 0.95 0.89 0.75
          let unresolved = [("ETF1", []), ("ETF2", ["UNKNOWN1"])]
          compOut <- mkComparisonOutput "test/input/20260101-IS-tech.csv" "test/input/20260102-SS-finance.csv" metrics unresolved
          coTimestamp compOut `shouldSatisfy` (\ts -> length ts > 0)
          coFile1 compOut `shouldBe` "test/input/20260101-IS-tech.csv"
          coFile2 compOut `shouldBe` "test/input/20260102-SS-finance.csv"
          coCosineSimilarity compOut `shouldBe` 0.95
          coWeightedJaccardSimilarity compOut `shouldBe` 0.89
          coOverlapRatio compOut `shouldBe` 0.75
          coUnresolvedIds compOut `shouldBe` unresolved

      describe "mkErrorOutput" $ do
        it "creates error output with timestamp and error file" $ do
          let errorList = [LoadPE "Validation error"]
          errOut <- mkErrorOutput "test/input/20260101-IS-tech.csv" errorList
          eoTimestamp errOut `shouldSatisfy` (\ts -> length ts > 0)
          eoFailedFile errOut `shouldBe` "test/input/20260101-IS-tech.csv"
          eoErrorList errOut `shouldBe` errorList

      describe "writeComparisonOutput" $ do
        it "writes comparison output CSV to test/output folder with descriptive column names" $ do
          let outputDir = "test" </> "output"
          let metrics = ComparisonMetrics 0.95 0.89 0.75
          compOut <- mkComparisonOutput "test/input/20260101-IS-tech.csv" "test/input/20260102-SS-finance.csv" metrics []
          writeComparisonOutput outputDir "demo-success" compOut
          let expectedFile = outputDir </> "comparison-demo-success.csv"
          fileExists <- doesFileExist expectedFile
          fileExists `shouldBe` True

      describe "writeErrorOutput" $ do
        it "writes error output CSV to test/output folder with descriptive column names" $ do
          let outputDir = "test" </> "output"
          let errorList = [LoadPE "Validation error"]
          errOut <- mkErrorOutput "test/input/20260101-IS-tech.csv" errorList
          writeErrorOutput outputDir "demo-error" errOut
          let expectedFile = outputDir </> "error-demo-error.csv"
          fileExists <- doesFileExist expectedFile
          fileExists `shouldBe` True

        it "produces the expected header and JSON error_list" $ do
          let outputDir = "test" </> "output"
          let errorList = [LoadPE "Validation error"]
          errOut <- mkErrorOutput "test/input/20260101-IS-tech.csv" errorList
          writeErrorOutput outputDir "json" errOut
          content <- readFile (outputDir </> "error-json.csv")
          let rows = lines content
          case rows of
            (header:_) -> header `shouldBe` "timestamp,error_file,error_list"
            []         -> expectationFailure "Expected at least one row"
          content `shouldSatisfy` isInfixOf "test/input/20260101-IS-tech.csv"
          content `shouldSatisfy` isInfixOf "Load"
          content `shouldSatisfy` isInfixOf "Validation error"

-- Helper function to clean output folder before tests
cleanupOutputFolder :: IO ()
cleanupOutputFolder = do
  let outputDir = "test" </> "output"
  createDirectoryIfMissing True outputDir
  files <- getDirectoryContents outputDir
  let filesToRemove = filter (not . (`elem` [".", ".."])) files
  mapM_ (removePathForcibly . (outputDir </>)) filesToRemove
