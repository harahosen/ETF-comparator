module Parser.Custom
  ( parseCustom
  ) where

import Domain.Types
import Parser.Common.ParserHelpers
import qualified Data.Text as T
import Data.Text (Text)

type Table = [[Text]]

parseCustom :: Table -> Either String RawETF
parseCustom (header : rows) = do
  fundId <- extractFundId header rows
  assetIx <- findColumnByPriority ["assetId"] header
  weightIx <- findColumnByPriority ["weight"] header
  holdings <- traverse (parseRow assetIx weightIx) rows
  Right (RawETF fundId Nothing holdings)

parseCustom [] =
  Left "Custom parser: empty table"

extractFundId :: [Text] -> [[Text]] -> Either String RawFundId
extractFundId header rows = do
  fundIx <- findColumnByPriority ["fundId"] header
  case rows of
    (firstRow : _) -> case safeIndex fundIx firstRow of
      Just fundText -> Right (RawFundId (T.unpack fundText))
      Nothing -> Left "Missing fund ID in first data row"
    [] -> Left "No data rows available for fund ID extraction"

parseRow :: Int -> Int -> [Text] -> Either String Holding
parseRow assetIx weightIx row = do
  asset <- maybe (Left "Missing asset id") Right (safeIndex assetIx row)
  wtxt <- maybe (Left "Missing weight") Right (safeIndex weightIx row)
  w <- parseDouble wtxt
  Right (Holding (RawAssetId (T.unpack asset)) Nothing (Weight w))

safeIndex :: Int -> [a] -> Maybe a
safeIndex i xs =
  if i < length xs then Just (xs !! i) else Nothing
