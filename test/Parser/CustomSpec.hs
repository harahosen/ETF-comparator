module Parser.CustomSpec (spec) where

import Test.Hspec
import Parser.Custom
import Domain.Types

spec :: Spec
spec = do
  describe "Parser.Custom" $ do
    it "parses a simple custom ETF table" $ do
      let table =
            [ ["fundId", "assetId", "weight"]
            , ["TEST", "AAPL", "0.6"]
            , ["TEST", "MSFT", "0.4"]
            ]

      let result = parseCustom table

      result `shouldBe`
        Right
          (RawETF
            (RawFundId "TEST")
            Nothing
            [ Holding (RawAssetId "AAPL") (Weight 0.6) Nothing
            , Holding (RawAssetId "MSFT") (Weight 0.4) Nothing
            ])
