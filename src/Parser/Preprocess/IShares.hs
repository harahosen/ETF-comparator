module Parser.IShares
  ( parseIShares
  ) where

import Parser.Common.Table
import Parser.Common.ParserHelpers
import Domain.Types

parseIShares :: FundId -> Table -> Either String RawETF
parseIShares fundId (header : rows) = do
  assetIx  <- findFirstCol assetCols header
  weightIx <- findFirstCol weightCols header
  holdings <- parseHoldings assetIx weightIx rows
  Right (RawETF fundId holdings)

parseIShares _ [] =
  Left "IShares: empty table"

assetCols :: [String]
assetCols = ["isin", "ticker", "symbol"]

weightCols :: [String]
weightCols = ["weight"]
