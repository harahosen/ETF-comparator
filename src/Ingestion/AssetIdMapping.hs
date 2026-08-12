module Ingestion.AssetIdMapping
  ( AssetIdMapping
  , resolveAssetId
  ) where

import qualified Data.Map.Strict as M
import Domain.Types

type AssetIdMapping = M.Map RawAssetId CanonicalAssetId

-- | Resolves raw asset identifiers into canonical ids
--   Resolution order is defined by mapping construction.
resolveAssetId :: AssetIdMapping -> RawAssetId -> Maybe CanonicalAssetId
resolveAssetId m rawId = M.lookup rawId m