module Parser.IShares
  ( parseIShares
  ) where

import Domain.Types
import Parser.Common.ParserHelpers
import Data.Text (Text)

type Table = [[Text]]

assetPriority :: [Text]
assetPriority =
  [ "isin"
  , "ticker"
  , "symbol"
  , "name"
  ]

weightPriority :: [Text]
weightPriority =
  [ "weight"
  , "weight %"
  , "weight (%)"
  , "weight(%)"
  , "market weight"
  ]

parseIShares :: FundId -> Table -> Either String RawETF
parseIShares fundId (header : rows) = do
  assetIx <- findColumnByPriority assetPriority header
  weightIx <- findColumnByPriority weightPriority header
  holdings <- traverse (parseRow assetIx weightIx) rows
  Right (RawETF fundId holdings)

parseIShares _ [] =
  Left "IShares parser: empty table"

parseRow :: Int -> Int -> [Text] -> Either String Holding
parseRow assetIx weightIx row = do
  asset <- maybe (Left "Missing asset id") Right (safeIndex assetIx row)
  wtxt <- maybe (Left "Missing weight") Right (safeIndex weightIx row)
  w <- parseDouble wtxt
  Right (Holding (RawAssetId asset) (Weight w) Nothing)

safeIndex :: Int -> [a] -> Maybe a
safeIndex i xs =
  if i < length xs then Just (xs !! i) else Nothing
