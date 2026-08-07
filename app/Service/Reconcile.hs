module Service.Reconcile where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Csv (decode, HasHeader (..))
import qualified Data.Map.Strict as M
import Data.Map (Map)
import qualified Data.Set as S
import Data.Set (Set)
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import Data.Char (isAlphaNum, toLower, toUpper)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.List (findIndex)
import System.Environment (getArgs)
import System.FilePath (takeBaseName, takeExtension, (</>))
import System.Directory (createDirectoryIfMissing)
import Ingestion.Xlsx (loadXlsx)
import Ingestion.FileMeta (deriveFileMetadata)
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

main :: IO ()
main = do
  args <- getArgs
  case args of
    [p, s] ->
      reconcileWithOut p s "Input"
    [p, s, o] ->
      reconcileWithOut p s o
    _ ->
      error "Usage: reconcile primary secondary [output-dir]"

reconcileWithOut :: FilePath -> FilePath -> FilePath -> IO ()
reconcileWithOut primary secondary outDir = do
  validate primary
  validate secondary
  createDirectoryIfMissing True outDir
  let pOut = outDir </> takeBaseName primary ++ "-adapted.csv"
      sOut = outDir </> takeBaseName secondary ++ "-adapted.csv"
  reconcileFiles primary secondary pOut sOut

validate :: FilePath -> IO ()
validate path =
  case deriveFileMetadata path of
    Left err -> error $ "Invalid input filename " ++ path ++ ": " ++ err
    Right _  -> return ()

reconcileFiles :: FilePath -> FilePath -> FilePath -> FilePath -> IO ()
reconcileFiles primaryPath secondaryPath pOut sOut = do
  primaryTable   <- cleanTable <$> loadInput primaryPath
  secondaryTable <- cleanTable <$> loadInput secondaryPath

  (pHeader, pRows) <- extract primaryPath primaryTable
  (sHeader, sRows) <- extract secondaryPath secondaryTable

  let primaryHoldings   = parseHoldings pHeader pRows
      secondaryHoldings = parseHoldings sHeader sRows
      slugMap           = buildSlugMap primaryHoldings
      primaryW          = canonicalize M.empty primaryHoldings
      secondaryW        = canonicalize slugMap secondaryHoldings

  writeCsv pOut  $ renderAdapted primaryW
  writeCsv sOut  $ renderAdapted secondaryW

  putStrLn $ "Wrote " ++ show (M.size primaryW) ++ " rows to " ++ pOut
  putStrLn $ "Wrote " ++ show (M.size secondaryW) ++ " rows to " ++ sOut

-- Input loading

loadInput :: FilePath -> IO Table
loadInput path =
  case map toLower (takeExtension path) of
    ".csv"  -> loadCsv path
    ".xlsx" -> loadXlsx path
    ext    -> error $ "Unsupported input extension: " ++ ext

loadCsv :: FilePath -> IO Table
loadCsv path = do
  bs <- LBS.readFile path
  case (decode NoHeader bs :: Either String (V.Vector (V.Vector BS.ByteString))) of
    Left err -> error $ "CSV load error: " ++ err
    Right rs ->
      return [ [ T.filter (/= '\xfeff') (TE.decodeUtf8Lenient field) | field <- V.toList record ]
             | record <- V.toList rs ]

cleanTable :: Table -> Table
cleanTable = filter (not . all (T.null . T.strip))

-- Header extraction

extract :: FilePath -> Table -> IO (Header, Table)
extract path table =
  case findHeader table of
    Nothing -> error $ "Could not find a header row in " ++ path
    Just (i, assetIdxs, nameIdxs, weightIdxs) ->
      return (Header assetIdxs nameIdxs weightIdxs, drop (i + 1) table)

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

-- Name normalization

normalizeName :: Text -> Text
normalizeName =
  T.unwords
  . filter (`S.notMember` stopWords)
  . T.words
  . T.map (\c -> if isAlphaNum c then toUpper c else ' ')
  . T.strip

-- Holding extraction

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

-- Rendering

renderAdapted :: Map Text Double -> Text
renderAdapted ws =
  T.unlines $ "ticker,weight"
            : [ k <> "," <> T.pack (show v) | (k, v) <- M.toAscList ws ]

writeCsv :: FilePath -> Text -> IO ()
writeCsv path content =
  LBS.writeFile path (LBS.fromStrict (TE.encodeUtf8 content))
