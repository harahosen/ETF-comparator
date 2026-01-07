module Parser.Common.ParserHelpers
  ( findFirstCol
  , findUniqueCol
  , parseHoldings
  ) where

import Parser.Common.Table
import Domain.Types

import Data.Char (toLower)
import Data.List (findIndex, findIndices)
import Text.Read (readMaybe)

assetCols :: [String]
assetCols = ["isin", "ticker", "symbol", "cusip", "asset", "id", "isin"]

weightCols :: [String]
weightCols = ["weight"]

marketWeightKeys :: [String]
marketWeightKeys =
  [ "market weight"
  , "market weight (%)"
  ]

genericWeightKeys :: [String]
genericWeightKeys =
  [ "weight"
  , "weight (%)"
  ]

notionalWeightKeys :: [String]
notionalWeightKeys =
  [ "notional weight"
  , "notional weight (%)"
  ]

lowerString :: String -> String
lowerString = map toLower

matchesAny :: [String] -> String -> Bool
matchesAny keys cell =
  let c = lowerString cell
  in any (`elem` words c) keys

at :: Int -> [a] -> Either String a
at i xs =
  maybe (Left "Column index out of bounds") Right (safeIndex i xs)

parseRow :: Int -> Int -> Row -> Either String Holding
parseRow assetIx weightIx row = do
  asset <- at assetIx row
  wStr  <- at weightIx row
  wVal  <- maybe (Left "Invalid weight value") Right (readMaybe wStr)
  Right (Holding (AssetId asset) (Weight wVal))

parseHoldings :: Int -> Int -> [Row] -> Either String [Holding]
parseHoldings assetIx weightIx =
  traverse (parseRow assetIx weightIx)
