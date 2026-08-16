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
            [ Holding (RawAssetId "AAPL") Nothing Nothing (Weight 0.6)
            , Holding (RawAssetId "MSFT") Nothing Nothing (Weight 0.4)
            ])
