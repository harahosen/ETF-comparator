module Parser.Custom
  ( parseCustom
  ) where

import Domain.Types
import Parser.Common.ParserHelpers
import qualified Data.Text as T
import Data.Text (Text)

type Table = [[Text]]

parseCustom :: FundId -> Table -> Either String RawETF
parseCustom fundId (header : rows) = do
  assetIx <- findColumnByPriority ["assetid"] header
  weightIx <- findColumnByPriority ["weight"] header
  holdings <- traverse (parseRow assetIx weightIx) rows
  Right (RawETF fundId holdings)

parseCustom _ [] =
  Left "Custom parser: empty table"

parseRow :: Int -> Int -> [Text] -> Either String Holding
parseRow assetIx weightIx row = do
  asset <- maybe (Left "Missing asset id") Right (safeIndex assetIx row)
  wtxt <- maybe (Left "Missing weight") Right (safeIndex weightIx row)
  w <- parseDouble wtxt
  Right (Holding (RawAssetId (T.unpack asset)) Nothing (Weight w))

safeIndex :: Int -> [a] -> Maybe a
safeIndex i xs =
  if i < length xs then Just (xs !! i) else Nothing
