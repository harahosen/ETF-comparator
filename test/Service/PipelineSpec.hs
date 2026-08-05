module Service.PipelineSpec (spec) where

import Test.Hspec
import Service.Config (loadConfigFromFile)
import Service.Pipeline (ComparisonMetrics(..), ComparisonResult(..), PipelineError(..), processComparisonWithConfig)

spec :: Spec
spec = do
  describe "Service.Pipeline" $ do
    describe "ComparisonResult" $ do
      it "creates successful comparison result" $ do
        let metrics = ComparisonMetrics 0.95 0.89 0.75
        let unresolved = [("ETF1", []), ("ETF2", ["UNKNOWN1"])]
        let result = ComparisonSuccess metrics unresolved
        case result of
          ComparisonSuccess m u -> do
            cosineSimilarityValue m `shouldBe` 0.95
            weightedJaccardSimilarityValue m `shouldBe` 0.89
            overlapRatioValue m `shouldBe` 0.75
            u `shouldBe` unresolved
          _ -> expectationFailure "Should be ComparisonSuccess"

      it "creates error comparison result with failed file and multiple errors" $ do
        let errors = [LoadPE "Test error", LoadPE "Second error"]
        let result = ComparisonError [("Test error", "file1.csv", errors)]
        case result of
          ComparisonError [(err, failedFile, allErrors)] -> do
            err `shouldBe` "Test error"
            failedFile `shouldBe` "file1.csv"
            allErrors `shouldBe` errors
          _ -> expectationFailure "Should be ComparisonError"

      it "extracts metrics from successful result" $ do
        let metrics = ComparisonMetrics 0.8 0.7 0.6
        let unresolved = [("20260101", []), ("20260102", [])]
        let result = ComparisonSuccess metrics unresolved
        case result of
          ComparisonSuccess m _ -> do
            cosineSimilarityValue m `shouldBe` 0.8
            weightedJaccardSimilarityValue m `shouldBe` 0.7
            overlapRatioValue m `shouldBe` 0.6
          _ -> expectationFailure "Should extract metrics"

      it "reports an invalid filename as a single LoadPE error and nothing else" $ do
        config <- either (error . ("Config: " ++)) return =<< loadConfigFromFile "test/config.yaml"
        result <- processComparisonWithConfig config "test/input/invalid-name.csv" "test/input/20260105-CF-correct.csv"
        case result of
          ComparisonError [(err, failedFile, allErrors)] -> do
            failedFile `shouldBe` "test/input/invalid-name.csv"
            length allErrors `shouldBe` 1
            case allErrors of
              [LoadPE loadErr] -> do
                err `shouldBe` loadErr
                take 12 loadErr `shouldBe` "Invalid date"
              _ -> expectationFailure "Should contain exactly one LoadPE error"
          _ -> expectationFailure "Should be ComparisonError with a single filename error"
