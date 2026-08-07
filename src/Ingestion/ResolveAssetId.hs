module Ingestion.ResolveAssetId
  ( resolveETFAssetIds
  ) where

import Domain.Types
import Ingestion.AssetIdMapping

resolveETFAssetIds :: AssetIdMapping -> RawETF -> (RawETF, [RawAssetId])
resolveETFAssetIds mapping (RawETF fundId hs) =
  (RawETF fundId resolved, [])
  where
    resolved = map resolve hs
    resolve h =
      case resolveAssetId mapping (holdingRawId h) of
        Just cid -> h { holdingCanonicalId = Just cid }
        Nothing  -> h { holdingCanonicalId = Just (CanonicalAssetId (unRawAssetId (holdingRawId h))) }

