module Ingestion.Reconcile
  ( reconcileTables
  ) where

import qualified Data.Map.Strict as M
import Data.Map (Map)
import qualified Data.Set as S
import Data.Set (Set)
import qualified Data.Text as T
import Data.Text (Text)
import Data.Char (isAlphaNum, toUpper)
import Data.List (findIndex)
import Data.Maybe (fromMaybe, listToMaybe, maybeToList)

import Parser.Common.ParserHelpers (normalizeHeader, parseDouble)

type Table = [[Text]]

data RawHolding = RawHolding
  { rhAsset  :: !Text
  , rhName   :: !(Maybe Text)
  , rhWeight :: !Double
  } deriving (Show)

data Header = Header
  { hAsset  :: ![Int]
  , hName   :: ![Int]
  , hWeight :: ![Int]
  } deriving (Show)

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

stopWords :: Set Text
stopWords = S.fromList
  [ "INC", "INCORPORATED", "CORP", "CORPORATION", "CO", "COMPANY", "PLC"
  , "LTD", "LIMITED", "LLC", "LP", "REIT", "TRUST", "CLASS", "A", "B", "C"
  , "W", "I", "WI", "WHEN", "ISSUED", "W-I", "W/I", "NEW", "SHARES"
  , "GROUP", "HOLDINGS", "HOLDING"
  ]

reconcileTables :: FilePath -> FilePath -> Table -> Table -> Either String (Table, Table)
reconcileTables primaryPath secondaryPath pTable sTable =
  case extract primaryPath (cleanTable pTable) of
    Left err -> Left err
    Right (pHeader, pRows) ->
      case extract secondaryPath (cleanTable sTable) of
        Left err -> Left err
        Right (sHeader, sRows) ->
          let primaryHoldings   = parseHoldings pHeader pRows
              secondaryHoldings = parseHoldings sHeader sRows
              slugMap           = buildSlugMap primaryHoldings
              primaryW          = canonicalize M.empty primaryHoldings
              secondaryW        = canonicalize slugMap secondaryHoldings
          in Right (tableFromMap primaryW, tableFromMap secondaryW)

cleanTable :: Table -> Table
cleanTable = filter (not . all (T.null . T.strip))

extract :: FilePath -> Table -> Either String (Header, Table)
extract path table =
  case findHeader table of
    Nothing -> Left ("Could not find a header row in " ++ path)
    Just (i, assetIdxs, nameIdxs, weightIdxs) ->
      Right (Header assetIdxs nameIdxs weightIdxs, drop (i + 1) table)

findHeader :: Table -> Maybe (Int, [Int], [Int], [Int])
findHeader = go 0
  where
    go _ []     = Nothing
    go i (r:rs) =
      let assetIdxs  = findColumnsByPriority assetPriorities r
          nameIdxs   = findColumnsByPriority namePriorities r
          weightIdxs = findColumnsByPriority weightPriorities r
      in if not (null assetIdxs) && not (null weightIdxs)
           then Just (i, assetIdxs, nameIdxs, weightIdxs)
           else go (i+1) rs

findColumnsByPriority :: [Text] -> [Text] -> [Int]
findColumnsByPriority priorities header =
  [ i | key <- priorities, i <- maybeToList (findIndex (matches key) header) ]
  where
    matches k h = normalizeHeader h == normalizeHeader k
    maybeToList = maybe [] pure

at :: Int -> [a] -> Maybe a
at _ []     = Nothing
at 0 (x:_)  = Just x
at n (_:xs) = at (n-1) xs

normalizeName :: Text -> Text
normalizeName =
  T.unwords
  . filter (`S.notMember` stopWords)
  . T.words
  . T.map (\c -> if isAlphaNum c then toUpper c else ' ')
  . T.strip

parseHoldings :: Header -> Table -> [RawHolding]
parseHoldings (Header (assetIdx:_) nameIdxs weightIdxs) rows =
  [ RawHolding (T.strip asset) (T.strip <$> mname) w
  | row <- rows
  , Just asset <- [at assetIdx row]
  , not (T.null (T.strip asset))
  , let mname = maybe Nothing (\i -> at i row) (listToMaybe nameIdxs)
  , let w = firstParseableWeight weightIdxs row
  , w > 0
  ]
parseHoldings _ _ = []

firstParseableWeight :: [Int] -> [Text] -> Double
firstParseableWeight [] _ = 0
firstParseableWeight (i:is) row =
  case at i row of
    Nothing -> firstParseableWeight is row
    Just t  -> case parseDouble' (T.strip t) of
                 Just w  -> w
                 Nothing -> firstParseableWeight is row

parseDouble' :: Text -> Maybe Double
parseDouble' t = either (const Nothing) Just (parseDouble t)

buildSlugMap :: [RawHolding] -> Map Text Text
buildSlugMap hs =
  M.fromListWith (\_ old -> old)
    [ (normalizeName n, T.strip (rhAsset h))
    | h <- hs
    , Just n <- [rhName h]
    , not (T.null n)
    ]

canonicalize :: Map Text Text -> [RawHolding] -> Map Text Double
canonicalize slugMap hs =
  M.fromListWith (+)
    [ (M.findWithDefault (T.strip (rhAsset h)) (normalizeName (fromMaybe "" (rhName h))) slugMap, rhWeight h)
    | h <- hs
    ]

tableFromMap :: Map Text Double -> Table
tableFromMap ws =
  ["ticker", "weight"] : [ [k, T.pack (show v)] | (k, v) <- M.toAscList ws ]
