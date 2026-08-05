module Domain.ValidationSpec (spec) where

import Test.Hspec
import Domain.Validation
import Domain.Types
import Domain.Errors

spec :: Spec
spec = do
  describe "Domain.Validation" $ do
    describe "validateRawETF" $ do
      it "validates a correct ETF" $ do
        let etf = RawETF (FundId "TEST")
              [ Holding (RawAssetId "AAPL") (Just (CanonicalAssetId "AAPL")) (Weight 0.6)
              , Holding (RawAssetId "MSFT") (Just (CanonicalAssetId "MSFT")) (Weight 0.4)
              ]
        validateRawETF etf `shouldBe` Right etf

      it "rejects ETF with empty holdings" $ do
        let etf = RawETF (FundId "TEST") []
        validateRawETF etf `shouldBe` Left [EmptyHoldings]

      it "rejects ETF with duplicate canonical IDs" $ do
        let etf = RawETF (FundId "TEST")
              [ Holding (RawAssetId "AAPL1") (Just (CanonicalAssetId "AAPL")) (Weight 0.3)
              , Holding (RawAssetId "AAPL2") (Just (CanonicalAssetId "AAPL")) (Weight 0.3)
              , Holding (RawAssetId "MSFT") (Just (CanonicalAssetId "MSFT")) (Weight 0.4)
              ]
        validateRawETF etf `shouldBe` Left [DuplicateHolding (CanonicalAssetId "AAPL")]

      it "rejects ETF with negative weights" $ do
        let etf = RawETF (FundId "TEST")
              [ Holding (RawAssetId "AAPL") (Just (CanonicalAssetId "AAPL")) (Weight (-0.1))
              , Holding (RawAssetId "MSFT") (Just (CanonicalAssetId "MSFT")) (Weight 1.1)
              ]
        case validateRawETF etf of
          Left errs -> NegativeWeight (CanonicalAssetId "AAPL") `elem` errs `shouldBe` True
          Right _ -> expectationFailure "Should have rejected negative weight"

      it "accepts ETF with unresolved holdings (no canonical ID)" $ do
        let etf = RawETF (FundId "TEST")
              [ Holding (RawAssetId "AAPL") Nothing (Weight 0.6)
              , Holding (RawAssetId "MSFT") Nothing (Weight 0.4)
              ]
        validateRawETF etf `shouldBe` Right etf

      it "validates ETF with holdings that sum to less than 1" $ do
        let etf = RawETF (FundId "TEST")
              [ Holding (RawAssetId "AAPL") (Just (CanonicalAssetId "AAPL")) (Weight 0.3)
              , Holding (RawAssetId "MSFT") (Just (CanonicalAssetId "MSFT")) (Weight 0.3)
              ]
        validateRawETF etf `shouldBe` Right etf

      it "validates ETF with holdings that sum to more than 1" $ do
        let etf = RawETF (FundId "TEST")
              [ Holding (RawAssetId "AAPL") (Just (CanonicalAssetId "AAPL")) (Weight 0.6)
              , Holding (RawAssetId "MSFT") (Just (CanonicalAssetId "MSFT")) (Weight 0.6)
              ]
        validateRawETF etf `shouldBe` Right etf

    describe "validateRawETFAllErrors" $ do
      it "collects all validation errors" $ do
        let etf = RawETF (FundId "TEST")
              [ Holding (RawAssetId "AAPL") (Just (CanonicalAssetId "AAPL")) (Weight (-0.5))
              , Holding (RawAssetId "MSFT") (Just (CanonicalAssetId "MSFT")) (Weight (-0.3))
              , Holding (RawAssetId "AAPL") (Just (CanonicalAssetId "AAPL")) (Weight 0.2)
              ]
        case validateRawETFAllErrors etf of
          Left errs -> length errs `shouldBe` 3  -- duplicate + 2 negative weights
          Right _ -> expectationFailure "Should have collected all errors"
