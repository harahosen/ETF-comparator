module Domain.ComparisonSpec (spec) where

import Test.Hspec
import Domain.Comparison
import Domain.Types
import qualified Data.Map.Strict as M

spec :: Spec
spec = do
  describe "Domain.Comparison" $ do
    describe "cosineSimilarity" $ do
      it "calculates cosine similarity for identical ETFs" $ do
        let etf1 = NormalizedETF (M.fromList [(CanonicalAssetId "AAPL", Weight 0.5), (CanonicalAssetId "MSFT", Weight 0.5)])
            etf2 = NormalizedETF (M.fromList [(CanonicalAssetId "AAPL", Weight 0.5), (CanonicalAssetId "MSFT", Weight 0.5)])
        cosineSimilarity etf1 etf2 `shouldSatisfy` (\x -> abs (x - 1.0) < 1e-10)

      it "calculates cosine similarity for orthogonal ETFs" $ do
        let etf1 = NormalizedETF (M.fromList [(CanonicalAssetId "AAPL", Weight 1.0)])
            etf2 = NormalizedETF (M.fromList [(CanonicalAssetId "MSFT", Weight 1.0)])
        cosineSimilarity etf1 etf2 `shouldBe` 0.0

      it "calculates cosine similarity for partially overlapping ETFs" $ do
        let etf1 = NormalizedETF (M.fromList [(CanonicalAssetId "AAPL", Weight 0.6), (CanonicalAssetId "MSFT", Weight 0.4)])
            etf2 = NormalizedETF (M.fromList [(CanonicalAssetId "AAPL", Weight 0.8), (CanonicalAssetId "GOOG", Weight 0.2)])
        cosineSimilarity etf1 etf2 `shouldSatisfy` (\x -> x > 0.0 && x < 1.0)

    describe "cosineDistance" $ do
      it "calculates cosine distance for identical ETFs" $ do
        let etf1 = NormalizedETF (M.fromList [(CanonicalAssetId "AAPL", Weight 0.5), (CanonicalAssetId "MSFT", Weight 0.5)])
            etf2 = NormalizedETF (M.fromList [(CanonicalAssetId "AAPL", Weight 0.5), (CanonicalAssetId "MSFT", Weight 0.5)])
        cosineDistance etf1 etf2 `shouldSatisfy` (\x -> abs (x - 0.0) < 1e-10)

      it "calculates cosine distance for orthogonal ETFs" $ do
        let etf1 = NormalizedETF (M.fromList [(CanonicalAssetId "AAPL", Weight 1.0)])
            etf2 = NormalizedETF (M.fromList [(CanonicalAssetId "MSFT", Weight 1.0)])
        cosineDistance etf1 etf2 `shouldBe` 1.0

    describe "weightedJaccardSimilarity" $ do
      it "calculates weighted Jaccard similarity for identical ETFs" $ do
        let etf1 = NormalizedETF (M.fromList [(CanonicalAssetId "AAPL", Weight 0.5), (CanonicalAssetId "MSFT", Weight 0.5)])
            etf2 = NormalizedETF (M.fromList [(CanonicalAssetId "AAPL", Weight 0.5), (CanonicalAssetId "MSFT", Weight 0.5)])
        weightedJaccardSimilarity etf1 etf2 `shouldBe` 1.0

      it "calculates weighted Jaccard similarity for disjoint ETFs" $ do
        let etf1 = NormalizedETF (M.fromList [(CanonicalAssetId "AAPL", Weight 1.0)])
            etf2 = NormalizedETF (M.fromList [(CanonicalAssetId "MSFT", Weight 1.0)])
        weightedJaccardSimilarity etf1 etf2 `shouldBe` 0.0

      it "calculates weighted Jaccard similarity for partially overlapping ETFs" $ do
        let etf1 = NormalizedETF (M.fromList [(CanonicalAssetId "AAPL", Weight 0.5), (CanonicalAssetId "MSFT", Weight 0.5)])
            etf2 = NormalizedETF (M.fromList [(CanonicalAssetId "AAPL", Weight 0.5), (CanonicalAssetId "GOOG", Weight 0.5)])
        weightedJaccardSimilarity etf1 etf2 `shouldBe` (1/3)

    describe "weightedJaccardDistance" $ do
      it "calculates weighted Jaccard distance for identical ETFs" $ do
        let etf1 = NormalizedETF (M.fromList [(CanonicalAssetId "AAPL", Weight 0.5), (CanonicalAssetId "MSFT", Weight 0.5)])
            etf2 = NormalizedETF (M.fromList [(CanonicalAssetId "AAPL", Weight 0.5), (CanonicalAssetId "MSFT", Weight 0.5)])
        weightedJaccardDistance etf1 etf2 `shouldBe` 0.0

      it "calculates weighted Jaccard distance for disjoint ETFs" $ do
        let etf1 = NormalizedETF (M.fromList [(CanonicalAssetId "AAPL", Weight 1.0)])
            etf2 = NormalizedETF (M.fromList [(CanonicalAssetId "MSFT", Weight 1.0)])
        weightedJaccardDistance etf1 etf2 `shouldBe` 1.0

    describe "overlapRatio" $ do
      it "calculates overlap ratio for identical ETFs" $ do
        let etf1 = NormalizedETF (M.fromList [(CanonicalAssetId "AAPL", Weight 0.5), (CanonicalAssetId "MSFT", Weight 0.5)])
            etf2 = NormalizedETF (M.fromList [(CanonicalAssetId "AAPL", Weight 0.5), (CanonicalAssetId "MSFT", Weight 0.5)])
        overlapRatio etf1 etf2 `shouldBe` 1.0

      it "calculates overlap ratio for ETFs where one is subset of another" $ do
        let etf1 = NormalizedETF (M.fromList [(CanonicalAssetId "AAPL", Weight 0.5), (CanonicalAssetId "MSFT", Weight 0.5)])
            etf2 = NormalizedETF (M.fromList [(CanonicalAssetId "AAPL", Weight 0.3), (CanonicalAssetId "MSFT", Weight 0.3), (CanonicalAssetId "GOOG", Weight 0.4)])
        overlapRatio etf1 etf2 `shouldBe` 0.6

      it "calculates overlap ratio for partially overlapping ETFs" $ do
        let etf1 = NormalizedETF (M.fromList [(CanonicalAssetId "AAPL", Weight 0.5), (CanonicalAssetId "MSFT", Weight 0.5)])
            etf2 = NormalizedETF (M.fromList [(CanonicalAssetId "AAPL", Weight 0.5), (CanonicalAssetId "GOOG", Weight 0.5)])
        overlapRatio etf1 etf2 `shouldBe` 0.5

      it "calculates overlap ratio for disjoint ETFs" $ do
        let etf1 = NormalizedETF (M.fromList [(CanonicalAssetId "AAPL", Weight 1.0)])
            etf2 = NormalizedETF (M.fromList [(CanonicalAssetId "MSFT", Weight 1.0)])
        overlapRatio etf1 etf2 `shouldBe` 0.0
