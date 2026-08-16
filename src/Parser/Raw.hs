module Parser.Raw
  ( parseRawTable
  ) where

import Domain.Types
import Parser.Common.ParserHelpers
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.Text (Text)
import Data.List (findIndex)
import Data.Maybe (maybeToList, mapMaybe)

type Table = [[Text]]

assetPriorities :: [Text]
assetPriorities =
  [ "ticker"
  , "symbol"
  , "isin"
  , "assetId"
  ]

namePriorities :: [Text]
namePriorities =
  [ "name"
  , "security name"
  ]

weightPriorities :: [Text]
weightPriorities =
  [ "market weight"
  , "notional weight"
  , "percent of fund"
  , "weight"
  , "weight %"
  , "weight (%)"
  , "weight(%)"
  ]

parseRawTable :: FundId -> Table -> Either String RawETF
parseRawTable fundId table =
  case cleanTable table of
    [] -> Left "Raw parser: empty table"
    t -> case findHeader t of
           Nothing -> Left "Raw parser: could not find a header row"
           Just (header, rows) -> do
             assetIx <- findColumnByPriority assetPriorities header
             let mnameIx = either (const Nothing) Just (findColumnByPriority namePriorities header)
             weightIx <- findColumnByPriority weightPriorities header
             let holdings = mapMaybe (parseRow assetIx mnameIx weightIx) rows
             let merged =
                   M.elems $ M.fromListWith mergeHoldings
                     [ (holdingRawId h, h) | h <- holdings ]
             Right (RawETF fundId merged)

findHeader :: Table -> Maybe ([Text], Table)
findHeader = go
  where
    go []     = Nothing
    go (r:rs) =
      let assetIdxs  = findColumnsByPriority assetPriorities r
          weightIdxs = findColumnsByPriority weightPriorities r
      in if not (null assetIdxs) && not (null weightIdxs)
           then Just (r, rs)
           else go rs

cleanTable :: Table -> Table
cleanTable = filter (not . all (T.null . T.strip))

findColumnsByPriority :: [Text] -> [Text] -> [Int]
findColumnsByPriority priorities header =
  [ i | key <- priorities, i <- maybeToList (findIndex (matches key) header) ]
  where
    matches k h = normalizeHeader h == normalizeHeader k
    maybeToList = maybe [] pure

mergeHoldings :: Holding -> Holding -> Holding
mergeHoldings h1 h2 =
  h1 { holdingWeight = holdingWeight h1 <> holdingWeight h2 }

parseRow :: Int -> Maybe Int -> Int -> [Text] -> Maybe Holding
parseRow assetIx mnameIx weightIx row =
  case safeIndex assetIx row of
    Nothing -> Nothing
    Just asset | T.null (T.strip asset) -> Nothing
    Just asset ->
      case safeIndex weightIx row of
        Nothing -> Nothing
        Just wtxt | T.null (T.strip wtxt) -> Nothing
        Just wtxt ->
          case parseDouble (T.strip wtxt) of
            Left _ -> Nothing
            Right w | w <= 0 -> Nothing
            Right w ->
              let mname = case mnameIx of
                            Nothing -> Nothing
                            Just i  -> case safeIndex i row of
                                         Just n | not (T.null (T.strip n)) -> Just (T.strip n)
                                         _ -> Nothing
              in Just (Holding (RawAssetId (T.unpack (T.strip asset))) Nothing mname (Weight w))
