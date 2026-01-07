module Parser.StateStreet
  ( parseStateStreet
  ) where

import Parser.Common.Table
import Parser.Common.ParserHelpers
import Domain.Types

findFirstCol :: [String] -> Row -> Either String Int
findFirstCol keys row =
  case findIndex (matchesAny keys) row of
    Nothing -> Left ("Missing required column: " ++ show keys)
    Just i  -> Right i

parseStateStreet :: FundId -> Table -> Either String RawETF
parseStateStreet fundId (header : rows) = do
  assetIx  <- findFirstCol assetCols header
  weightIx <- findWeightCol header
  holdings <- traverse (parseRow assetIx weightIx) rows
  Right (RawETF fundId holdings)
parseStateStreet _ [] =
  Left "StateStreet: empty table"

