module Parser.Reconciled
  ( parseReconciled
  ) where

import Domain.Types
import Parser.Common.ParserHelpers
import qualified Data.Text as T
import Data.Text (Text)

type Table = [[Text]]

parseReconciled :: FundId -> Table -> Either String RawETF
parseReconciled fundId (header : rows) = do
  assetIx <- findColumnByPriority ["ticker"] header
  weightIx <- findColumnByPriority ["weight"] header
  holdings <- traverse (parseRow assetIx weightIx) rows
  Right (RawETF fundId holdings)

parseReconciled _ [] =
  Left "Reconciled parser: empty table"

parseRow :: Int -> Int -> [Text] -> Either String Holding
parseRow assetIx weightIx row = do
  asset <- maybe (Left "Missing asset id") Right (safeIndex assetIx row)
  wtxt <- maybe (Left "Missing weight") Right (safeIndex weightIx row)
  w <- parseDouble wtxt
  Right (Holding (RawAssetId (T.unpack asset)) Nothing (Weight w))

safeIndex :: Int -> [a] -> Maybe a
safeIndex i xs =
  if i < length xs then Just (xs !! i) else Nothing
