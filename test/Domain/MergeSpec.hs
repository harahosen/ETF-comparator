module Domain.MergeSpec (spec) where

import Test.Hspec
import Domain.Merge
import Domain.Types
import qualified Data.Map.Strict as M

spec :: Spec
spec = do
  describe "Domain.Merge" $ do
    describe "mergeResolvedHoldings" $ do
      it "merges holdings with same canonical ID" $ do
        let etf = RawETF (FundId "TEST")
              [ Holding (RawAssetId "AAPL1") (Just (CanonicalAssetId "AAPL")) (Weight 0.3)
              , Holding (RawAssetId "AAPL2") (Just (CanonicalAssetId "AAPL")) (Weight 0.3)
              , Holding (RawAssetId "MSFT") (Just (CanonicalAssetId "MSFT")) (Weight 0.4)
              ]
        let merged = mergeResolvedHoldings etf
        length (etfHoldings merged) `shouldBe` 2

      it "sums weights when merging holdings" $ do
        let etf = RawETF (FundId "TEST")
              [ Holding (RawAssetId "AAPL1") (Just (CanonicalAssetId "AAPL")) (Weight 0.3)
              , Holding (RawAssetId "AAPL2") (Just (CanonicalAssetId "AAPL")) (Weight 0.3)
              ]
        let merged = mergeResolvedHoldings etf
        let aaplHoldings = filter (\h -> holdingCanonicalId h == Just (CanonicalAssetId "AAPL")) (etfHoldings merged)
        case aaplHoldings of
          [h] -> holdingWeight h `shouldBe` Weight 0.6
          _ -> expectationFailure "Expected exactly one AAPL holding after merge"

      it "preserves holdings without canonical ID" $ do
        let etf = RawETF (FundId "TEST")
              [ Holding (RawAssetId "AAPL") (Just (CanonicalAssetId "AAPL")) (Weight 0.6)
              , Holding (RawAssetId "UNKNOWN") Nothing (Weight 0.4)
              ]
        let merged = mergeResolvedHoldings etf
        length (etfHoldings merged) `shouldBe` 2
        let unresolved = filter (\h -> holdingCanonicalId h == Nothing) (etfHoldings merged)
        length unresolved `shouldBe` 1

      it "does not merge holdings with different canonical IDs" $ do
        let etf = RawETF (FundId "TEST")
              [ Holding (RawAssetId "AAPL") (Just (CanonicalAssetId "AAPL")) (Weight 0.6)
              , Holding (RawAssetId "MSFT") (Just (CanonicalAssetId "MSFT")) (Weight 0.4)
              ]
        let merged = mergeResolvedHoldings etf
        length (etfHoldings merged) `shouldBe` 2

      it "handles ETF with only unresolved holdings" $ do
        let etf = RawETF (FundId "TEST")
              [ Holding (RawAssetId "UNKNOWN1") Nothing (Weight 0.6)
              , Holding (RawAssetId "UNKNOWN2") Nothing (Weight 0.4)
              ]
        let merged = mergeResolvedHoldings etf
        length (etfHoldings merged) `shouldBe` 2

      it "handles ETF with only resolved holdings" $ do
        let etf = RawETF (FundId "TEST")
              [ Holding (RawAssetId "AAPL") (Just (CanonicalAssetId "AAPL")) (Weight 0.6)
              , Holding (RawAssetId "MSFT") (Just (CanonicalAssetId "MSFT")) (Weight 0.4)
              ]
        let merged = mergeResolvedHoldings etf
        length (etfHoldings merged) `shouldBe` 2
        all (\h -> holdingCanonicalId h /= Nothing) (etfHoldings merged) `shouldBe` True

      it "preserves fund ID" $ do
        let etf = RawETF (FundId "TEST")
              [ Holding (RawAssetId "AAPL") (Just (CanonicalAssetId "AAPL")) (Weight 0.6)
              ]
        let merged = mergeResolvedHoldings etf
        etfFundId merged `shouldBe` FundId "TEST"
