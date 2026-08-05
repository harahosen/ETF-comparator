module Ingestion.FileMetaSpec (spec) where

import Test.Hspec
import Ingestion.FileMeta

spec :: Spec
spec = do
  describe "Ingestion.FileMeta" $ do
    describe "deriveFileMetadata" $ do
      it "parses valid iShares CSV filename" $ do
        let path = "20260101-IS-tech.csv"
        case deriveFileMetadata path of
          Right meta -> do
            fmDate meta `shouldBe` "20260101"
            fmProvider meta `shouldBe` IS
            fmFormat meta `shouldBe` CSV
          Left err -> expectationFailure $ "Should parse valid filename: " ++ err

      it "parses valid Custom CSV filename" $ do
        let path = "20260103-CF-mixed.csv"
        case deriveFileMetadata path of
          Right meta -> do
            fmDate meta `shouldBe` "20260103"
            fmProvider meta `shouldBe` CF
            fmFormat meta `shouldBe` CSV
          Left err -> expectationFailure $ "Should parse valid filename: " ++ err

      it "rejects invalid date format" $ do
        let path = "2026-01-IS-tech.csv"
        deriveFileMetadata path `shouldBe` Left "Invalid date in filename: 2026"

      it "rejects unknown provider code" $ do
        let path = "20260101-XX-tech.csv"
        deriveFileMetadata path `shouldBe` Left "Unknown provider code: XX"

      it "rejects unsupported file extension" $ do
        let path = "20260101-IS-tech.txt"
        deriveFileMetadata path `shouldBe` Left "Unsupported file extension: .txt"

      it "rejects malformed filename" $ do
        let path = "invalid-filename.csv"
        case deriveFileMetadata path of
          Left err -> err `shouldContain` "Invalid date"
          Right _ -> expectationFailure "Should reject malformed filename"

      it "preserves full path in metadata" $ do
        let path = "data/input/20260101-IS-tech.csv"
        case deriveFileMetadata path of
          Right meta -> fmPath meta `shouldBe` "data/input/20260101-IS-tech.csv"
          Left err -> expectationFailure $ "Should preserve path: " ++ err
