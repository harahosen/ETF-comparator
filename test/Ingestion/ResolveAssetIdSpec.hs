module Ingestion.ResolveAssetIdSpec (spec) where

import Test.Hspec
import Ingestion.ResolveAssetId
import Ingestion.AssetIdMapping
import Domain.Types
import qualified Data.Map.Strict as M

spec :: Spec
spec = do
  describe "Ingestion.ResolveAssetId" $ do
    describe "resolveETFAssetIds" $ do
      it "resolves all asset IDs when mapping exists" $ do
        let mapping = M.fromList
              [ (RawAssetId "AAPL_RAW", CanonicalAssetId "AAPL")
              , (RawAssetId "MSFT_RAW", CanonicalAssetId "MSFT")
              ]
        let etf = RawETF (FundId "TEST")
              [ Holding (RawAssetId "AAPL_RAW") Nothing (Weight 0.6)
              , Holding (RawAssetId "MSFT_RAW") Nothing (Weight 0.4)
              ]
        let (resolved, unresolved) = resolveETFAssetIds mapping etf
        length unresolved `shouldBe` 0
        all (\h -> holdingCanonicalId h /= Nothing) (etfHoldings resolved) `shouldBe` True

      it "keeps unresolved asset IDs when mapping doesn't exist" $ do
        let mapping = M.fromList
              [ (RawAssetId "AAPL_RAW", CanonicalAssetId "AAPL")
              ]
        let etf = RawETF (FundId "TEST")
              [ Holding (RawAssetId "AAPL_RAW") Nothing (Weight 0.6)
              , Holding (RawAssetId "UNKNOWN_RAW") Nothing (Weight 0.4)
              ]
        let (resolved, unresolved) = resolveETFAssetIds mapping etf
        length unresolved `shouldBe` 1
        unresolved `shouldBe` [RawAssetId "UNKNOWN_RAW"]

      it "handles empty mapping" $ do
        let mapping = M.empty :: AssetIdMapping
        let etf = RawETF (FundId "TEST")
              [ Holding (RawAssetId "AAPL") Nothing (Weight 0.6)
              , Holding (RawAssetId "MSFT") Nothing (Weight 0.4)
              ]
        let (resolved, unresolved) = resolveETFAssetIds mapping etf
        length unresolved `shouldBe` 2

      it "handles empty ETF" $ do
        let mapping = M.fromList
              [ (RawAssetId "AAPL", CanonicalAssetId "AAPL")
              ]
        let etf = RawETF (FundId "TEST") []
        let (resolved, unresolved) = resolveETFAssetIds mapping etf
        length (etfHoldings resolved) `shouldBe` 0
        length unresolved `shouldBe` 0

      it "preserves fund ID" $ do
        let mapping = M.fromList
              [ (RawAssetId "AAPL", CanonicalAssetId "AAPL")
              ]
        let etf = RawETF (FundId "TEST")
              [ Holding (RawAssetId "AAPL") Nothing (Weight 1.0)
              ]
        let (resolved, _) = resolveETFAssetIds mapping etf
        etfFundId resolved `shouldBe` FundId "TEST"

      it "preserves weights" $ do
        let mapping = M.fromList
              [ (RawAssetId "AAPL", CanonicalAssetId "AAPL")
              , (RawAssetId "MSFT", CanonicalAssetId "MSFT")
              ]
        let etf = RawETF (FundId "TEST")
              [ Holding (RawAssetId "AAPL") Nothing (Weight 0.6)
              , Holding (RawAssetId "MSFT") Nothing (Weight 0.4)
              ]
        let (resolved, _) = resolveETFAssetIds mapping etf
        let weights = map holdingWeight (etfHoldings resolved)
        weights `shouldMatchList` [Weight 0.6, Weight 0.4]
