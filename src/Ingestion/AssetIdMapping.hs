module Ingestion.AssetIdMapping
  ( AssetIdMapping
  , resolveAssetId
  ) where

import qualified Data.Map.Strict as M
import Domain.Types

type AssetIdMapping =
  M.Map RawAssetId CanonicalAssetId

loadAssetIdMapping :: FilePath -> IO AssetIdMapping
loadAssetIdMapping path = do
  content <- TIO.readFile path
  let rows = drop 1 (T.lines content)  -- skip header
  pure $ M.fromList (map parseRow rows)
  where
    parseRow line =
      case T.splitOn "," line of
        [raw, canon] -> ( RawAssetId (T.strip raw), CanonicalAssetId (T.strip canon) )
        _ -> error ("Invalid mapping row: " <> T.unpack line)

resolveAssetId :: AssetIdMapping -> RawAssetId -> Maybe CanonicalAssetId
resolveAssetId mapping rawId = M.lookup rawId mapping