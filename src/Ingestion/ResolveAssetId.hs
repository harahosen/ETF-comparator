module Ingestion.ResolveAssetId
  ( resolveETFAssetIds
  ) where

import Domain.Types
import Ingestion.AssetIdMapping

resolveETFAssetId :: AssetIdMapping -> RawETF -> RawETF
resolveETFAssetIds mapping (RawETF fundId holdings) =
  RawETF fundId (map resolveHolding holdings)
  where
    resolveHolding h = h { holdingCanonicalId = resolveAssetId mapping (holdingRawId h) }
