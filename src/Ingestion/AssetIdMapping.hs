module Ingestion.AssetIdMapping
  ( AssetIdMapping
  , resolve
  ) where

import qualified Data.Map.Strict as M
import Domain.Types

type AssetIdMapping =M.Map RawAssetId CanonicalAssetId

-- | Resolves raw asset identifiers into canonical ids
--   Resolution order is defined by mapping construction.
resolve :: AssetIdMapping -> RawAssetId -> Maybe CanonicalAssetId
resolve = M.lookup