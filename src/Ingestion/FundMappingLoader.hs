module Ingestion.FundMappingLoader
  ( loadFundMapping
  ) where

import Ingestion.FundIdMapping
import Domain.Types
import qualified Data.ByteString as BS
import Data.Csv (decodeByName)
import qualified Data.Map.Strict as M
import qualified Data.Vector as V
import Data.ByteString.Char8 (unpack)

loadFundMapping :: FilePath -> IO (Either String FundIdMapping)
loadFundMapping path = do
  result <- decodeByName <$> BS.readFile path
  case result of
    Left err -> return $ Left ("Fund mapping file parsing error: " ++ err)
    Right (header, records) -> do
      -- Assuming CSV format: rawFundId,canonicalFundId
      let recordList = V.toList records
          mapping = foldr (\record acc -> 
                           let fields = V.toList record
                               raw = head fields
                               canonical = fields !! 1
                           in M.insert (RawFundId (unpack raw)) (CanonicalFundId (unpack canonical)) acc)
                         M.empty
                         recordList
      return $ Right mapping
