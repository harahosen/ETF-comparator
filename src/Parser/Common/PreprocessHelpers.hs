module Parser.Common.PreprocessHelpers
  ( assetIdKeywords
  , weightKeywords
  , isHeaderRow
  , isDataRow
  , findHeader
  ) where

import Parser.Common.Table
import Data.Char (toLower)
import Data.List (findIndex, isInfixOf)

assetIdKeywords :: [String]
assetIdKeywords =
  [ "isin"
  , "ticker"
  , "symbol"
  , "security"
  , "cusip"
  ]

weightKeywords :: [String]
weightKeywords =
  [ "weight"
  , "market weight"
  , "notional weight"
  ]

normalize :: String -> String
normalize = map toLower

containsAny :: [String] -> String -> Bool
containsAny keywords cell =
  let cell' = normalize cell
  in any (`isInfixOf` cell') keywords

isHeaderRow :: Row -> Bool
isHeaderRow row =
     any (containsAny assetIdKeywords) row
  && any (containsAny weightKeywords) row

isDataRow :: Int -> Row -> Bool
isDataRow expectedCols row =
  length row == expectedCols
  && not (all null row)

findHeader :: Table -> Either String (Int, Row)
findHeader rows =
  case findIndex isHeaderRow rows of
    Nothing -> Left "Holdings header not found"
    Just i  -> Right (i, rows !! i)
