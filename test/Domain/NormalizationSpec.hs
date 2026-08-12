module Domain.NormalizationSpec (spec) where

import Test.Hspec
import Domain.Normalization
import Domain.Types
import Domain.Errors
import qualified Data.Map.Strict as M

spec :: Spec
spec = do
  describe "Domain.Normalization" $ do
    describe "normalizeETF" $ do
      it "normalizes ETF with weights summing to 1" $ do
        let etf = RawETF (FundId "TEST")
              [ Holding (RawAssetId "AAPL") (Just (CanonicalAssetId "AAPL")) (Weight 0.6)
              , Holding (RawAssetId "MSFT") (Just (CanonicalAssetId "MSFT")) (Weight 0.4)
              ]
        case normalizeETF etf of
          Right (NormalizedETF m) -> sum (map unWeight (M.elems m)) `shouldSatisfy` (\total -> abs (total - 1.0) < 1e-6)
          Left err -> expectationFailure $ "Should normalize: " ++ show err

      it "normalizes ETF with weights not summing to 1" $ do
        let etf = RawETF (FundId "TEST")
              [ Holding (RawAssetId "AAPL") (Just (CanonicalAssetId "AAPL")) (Weight 0.3)
              , Holding (RawAssetId "MSFT") (Just (CanonicalAssetId "MSFT")) (Weight 0.3)
              ]
        case normalizeETF etf of
          Right (NormalizedETF m) -> sum (map unWeight (M.elems m)) `shouldSatisfy` (\total -> abs (total - 1.0) < 1e-6)
          Left err -> expectationFailure $ "Should normalize: " ++ show err

      it "rejects ETF with zero total weight" $ do
        let etf = RawETF (FundId "TEST")
              [ Holding (RawAssetId "AAPL") (Just (CanonicalAssetId "AAPL")) (Weight 0.0)
              , Holding (RawAssetId "MSFT") (Just (CanonicalAssetId "MSFT")) (Weight 0.0)
              ]
        normalizeETF etf `shouldBe` Left ZeroTotalWeight

    describe "normalizeETFWithTolerance" $ do
      it "accepts ETF with weights within tolerance" $ do
        let etf = RawETF (FundId "TEST")
              [ Holding (RawAssetId "AAPL") (Just (CanonicalAssetId "AAPL")) (Weight 0.6000005)
              , Holding (RawAssetId "MSFT") (Just (CanonicalAssetId "MSFT")) (Weight 0.3999995)
              ]
        case normalizeETFWithTolerance 1e-4 etf of
          Right (NormalizedETF m) -> do
            M.lookup (CanonicalAssetId "AAPL") m `shouldBe` Just (Weight 0.6000005)
            M.lookup (CanonicalAssetId "MSFT") m `shouldBe` Just (Weight 0.3999995)
          Left err -> expectationFailure $ "Should accept within tolerance: " ++ show err

      it "normalizes ETF with weights outside tolerance" $ do
        let etf = RawETF (FundId "TEST")
              [ Holding (RawAssetId "AAPL") (Just (CanonicalAssetId "AAPL")) (Weight 0.3)
              , Holding (RawAssetId "MSFT") (Just (CanonicalAssetId "MSFT")) (Weight 0.3)
              ]
        case normalizeETFWithTolerance 1e-6 etf of
          Right (NormalizedETF m) -> sum (map unWeight (M.elems m)) `shouldSatisfy` (\total -> abs (total - 1.0) < 1e-6)
          Left err -> expectationFailure $ "Should normalize outside tolerance: " ++ show err

    describe "isNormalized" $ do
      it "returns True for normalized ETF" $ do
        let etf = RawETF (FundId "TEST")
              [ Holding (RawAssetId "AAPL") (Just (CanonicalAssetId "AAPL")) (Weight 0.6)
              , Holding (RawAssetId "MSFT") (Just (CanonicalAssetId "MSFT")) (Weight 0.4)
              ]
        isNormalized etf `shouldBe` True

      it "returns False for non-normalized ETF" $ do
        let etf = RawETF (FundId "TEST")
              [ Holding (RawAssetId "AAPL") (Just (CanonicalAssetId "AAPL")) (Weight 0.3)
              , Holding (RawAssetId "MSFT") (Just (CanonicalAssetId "MSFT")) (Weight 0.3)
              ]
        isNormalized etf `shouldBe` False
