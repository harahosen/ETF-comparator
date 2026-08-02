module Ingestion.MappingLoader
  ( loadAssetMapping
  ) where

import Ingestion.AssetIdMapping
import Domain.Types
import qualified Data.ByteString as BS
import Data.Csv (decodeByName)
import qualified Data.Map.Strict as M
import qualified Data.Vector as V
import Data.ByteString.Char8 (unpack)

loadAssetMapping :: FilePath -> IO (Either String AssetIdMapping)
loadAssetMapping path = do
  result <- decodeByName <$> BS.readFile path
  case result of
    Left err -> return $ Left ("Mapping file parsing error: " ++ err)
    Right (header, records) -> do
      -- Assuming CSV format: rawId,canonicalId
      let recordList = V.toList records
          mapping = foldr (\record acc -> 
                           let fields = V.toList record
                               raw = head fields
                               canonical = fields !! 1
                           in M.insert (RawAssetId (unpack raw)) (CanonicalAssetId (unpack canonical)) acc)
                         M.empty
                         recordList
      return $ Right mapping