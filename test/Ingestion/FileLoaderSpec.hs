module Ingestion.FileLoaderSpec (spec) where

import Test.Hspec
import Ingestion.FileLoader
import Domain.Types
import System.FilePath ((</>))

spec :: Spec
spec = do
  describe "Ingestion.FileLoader" $ do
    describe "loadETF" $ do
      it "returns error for non-existent file" $ do
        let filePath = "test" </> "input" </> "nonexistent.csv"
        result <- loadETF filePath
        case result of
          Left _ -> return () -- Expected to fail
          Right _ -> expectationFailure "Should return error for non-existent file"

      it "returns error for invalid filename format" $ do
        let filePath = "test" </> "input" </> "20260101-XX-tech.csv"
        result <- loadETF filePath
        case result of
          Left _ -> return () -- Expected to fail
          Right _ -> expectationFailure "Should return error for invalid filename"
