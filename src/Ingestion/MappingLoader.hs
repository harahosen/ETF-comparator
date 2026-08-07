module Ingestion.MappingLoader
  ( loadAssetMapping
  ) where

import Ingestion.AssetIdMapping
import Domain.Types
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Char8 as BC
import Data.Csv (decode, HasHeader(..))
import qualified Data.Map.Strict as M
import qualified Data.Vector as V
import System.Directory (doesFileExist)

loadAssetMapping :: FilePath -> IO (Either String AssetIdMapping)
loadAssetMapping path = do
  exists <- doesFileExist path
  if not exists
    then return $ Right M.empty
    else do
      bs <- LBS.readFile path
      let result = decode HasHeader bs
      case result of
        Left err -> return $ Left ("Mapping file parsing error: " ++ err)
        Right records -> do
          let recordList = V.toList records
              mapping = foldr (\record acc ->
                               case V.toList record of
                                 [raw, canonical] -> M.insert (RawAssetId (BC.unpack raw)) (CanonicalAssetId (BC.unpack canonical)) acc
                                 _ -> acc)
                             M.empty
                             recordList
          return $ Right mapping