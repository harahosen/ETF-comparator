module Parser.IShares
  ( parseIShares
  ) where

import Parser.Common.Table
import Parser.Common.ParserHelpers
import Domain.Types

import Data.Char (toLower)
import Data.List (findIndex)
import Text.Read (readMaybe)

parseIShares :: FundId -> Table -> Either String RawETF
parseIShares fundId (header : rows) = do
  assetIx  <- findCol assetCols header
  weightIx <- findWeightCol header
  holdings <- traverse (parseRow assetIx weightIx) rows
  Right (RawETF fundId holdings)
parseIShares _ [] =
  Left "IShares: empty table"

findCol :: [String] -> Row -> Either String Int
findCol keys row =
  case findIndex (matches keys) row of
    Nothing -> Left ("Missing required column: " ++ show keys)
    Just i  -> Right i

matches :: [String] -> String -> Bool
matches keys cell =
  let c = map toLower cell
  in any (`elem` words c) keys

parseRow :: Int -> Int -> Row -> Either String Holding
parseRow assetIx weightIx row = do
  assetTxt  <- at assetIx row
  weightTxt <- at weightIx row
  weight <- parseWeight weightTxt
  Right Holding
    { holdingRawId       = RawAssetId assetTxt
    , holdingCanonicalId = Nothing
    , holdingWeight      = weight
    }

at :: Int -> [a] -> Either String a
at i xs =
  maybe (Left "Column index out of bounds") Right
        (safeIndex i xs)

safeIndex :: Int -> [a] -> Maybe a
safeIndex 0 (x:_) = Just x
safeIndex n (_:xs) = safeIndex (n-1) xs
safeIndex _ [] = Nothing
