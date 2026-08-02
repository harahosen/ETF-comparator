module Parser.ISharesSpec (spec) where

import Test.Hspec
import Parser.IShares
import Domain.Types

spec :: Spec
spec = do
  describe "Parser.IShares" $ do
    it "parses a simple iShares ETF table" $ do
      let table =
            [ ["ticker", "weight"]
            , ["AAPL", "0.6"]
            , ["MSFT", "0.4"]
            ]

      let result = parseIShares (FundId "TEST") table

      result `shouldBe`
        Right
          (RawETF
            (FundId "TEST")
            [ Holding (RawAssetId "AAPL") (Weight 0.6) Nothing
            , Holding (RawAssetId "MSFT") (Weight 0.4) Nothing
            ])
