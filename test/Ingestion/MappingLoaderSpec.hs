module Ingestion.MappingLoaderSpec (spec) where

import Test.Hspec
import Ingestion.MappingLoader
import Ingestion.AssetIdMapping
import Domain.Types
import qualified Data.Map.Strict as M
import System.FilePath ((</>))
import System.IO (writeFile)
import System.Directory (removeFile)

spec :: Spec
spec = do
  describe "Ingestion.MappingLoader" $ do
    describe "loadAssetMapping" $ do
      it "loads valid asset mapping file" $ do
        let mappingPath = "test" </> "input" </> "asset-mapping.csv"
        result <- loadAssetMapping mappingPath
        case result of
          Right mapping -> do
            M.size mapping `shouldBe` 7
            M.lookup (RawAssetId "AAPL") mapping `shouldBe` Just (CanonicalAssetId "AAPL")
            M.lookup (RawAssetId "US0378331005") mapping `shouldBe` Just (CanonicalAssetId "AAPL")
          Left err -> expectationFailure $ "Should load valid mapping: " ++ err

      it "handles empty mapping file" $ do
        -- Create temporary empty mapping file
        let emptyPath = "test" </> "input" </> "empty-mapping.csv"
        writeFile emptyPath "raw,canonical\n"
        result <- loadAssetMapping emptyPath
        case result of
          Right mapping -> M.size mapping `shouldBe` 0
          Left err -> expectationFailure $ "Should handle empty mapping: " ++ err
        -- Clean up
        removeFile emptyPath

      it "preserves mapping relationships" $ do
        let mappingPath = "test" </> "input" </> "asset-mapping.csv"
        result <- loadAssetMapping mappingPath
        case result of
          Right mapping -> do
            M.lookup (RawAssetId "MSFT") mapping `shouldBe` Just (CanonicalAssetId "MSFT")
            M.lookup (RawAssetId "US5949181045") mapping `shouldBe` Just (CanonicalAssetId "MSFT")
          Left err -> expectationFailure $ "Should preserve relationships: " ++ err
