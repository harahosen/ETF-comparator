module Ingestion.AssetIdMapping
  ( AssetIdMapping
  , resolveAssetId
  ) where

import qualified Data.Map.Strict as M
import Domain.Types

type AssetIdMapping =
  M.Map RawAssetId CanonicalAssetId

resolveAssetId :: AssetIdMapping -> RawAssetId -> Maybe CanonicalAssetId
resolveAssetId mapping rawId = M.lookup rawId mapping