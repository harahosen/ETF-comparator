module Parser.CustomSpec (spec) where

import Test.Hspec
import Parser.Custom
import Domain.Types

spec :: Spec
spec = do
  describe "Parser.Custom" $ do
    it "parses a simple custom ETF table" $ do
      let table =
            [ ["assetId", "weight"]
            , ["AAPL", "0.6"]
            , ["MSFT", "0.4"]
            ]

      let result = parseCustom (FundId "TEST") table

      result `shouldBe`
        Right
          (RawETF
            (FundId "TEST")
            [ Holding (RawAssetId "AAPL") Nothing (Weight 0.6)
            , Holding (RawAssetId "MSFT") Nothing (Weight 0.4)
            ])
