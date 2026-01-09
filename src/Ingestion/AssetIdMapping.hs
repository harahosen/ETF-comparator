module Ingestion.AssetIdMapping
  ( AssetIdMapping
  , resolve
  ) where

import Domain.Types

-- | Resolves raw asset identifiers into canonical ids
--   Resolution order is defined by mapping construction.
resolve :: AssetIdMapping -> RawAssetId -> Maybe CanonicalAssetId
resolve = M.lookup
