module Parser.Custom
  ( parseCustom
  ) where

import Parser.Common.Table
import Parser.Common.ParserHelpers
import Domain.Types

findUniqueCol :: [String] -> Row -> Either String Int
findUniqueCol keys row =
  case findIndices (matchesAny keys) row of
    [i] -> Right i
    [] -> Left ("Missing required column: " ++ show keys)
    _  -> Left ("Ambiguous columns: " ++ show keys)

parseCustom :: FundId -> Table -> Either String RawETF
parseCustom fundId (header : rows) = do
  assetIx  <- findUniqueCol assetCols header
  weightIx <- findUniqueCol weightCols header
  holdings <- traverse (parseRow assetIx weightIx) rows
  Right (RawETF fundId holdings)
parseCustom _ [] =
  Left "Custom: empty table"