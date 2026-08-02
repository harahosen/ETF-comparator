module Parser.StateStreetSpec (spec) where

import Test.Hspec
import Parser.StateStreet
import Domain.Types

spec :: Spec
spec = do
  describe "Parser.StateStreet" $ do
    it "parses a simple StateStreet ETF table" $ do
      let table =
            [ ["ticker", "weight"]
            , ["AAPL", "0.6"]
            , ["MSFT", "0.4"]
            ]

      let result = parseStateStreet (FundId "TEST") table

      result `shouldBe`
        Right
          (RawETF
            (FundId "TEST")
            [ Holding (RawAssetId "AAPL") (Weight 0.6) Nothing
            , Holding (RawAssetId "MSFT") (Weight 0.4) Nothing
            ])
