module Ingestion.ResolveAssetId
  ( resolveETFAssetIds
  ) where

import Domain.Types
import Ingestion.AssetIdMapping

resolveETFAssetIds :: AssetIdMapping -> RawETF -> RawETF
resolveETFAssetIds mapping (RawETF fundId hs) =
  RawETF fundId (map resolve hs)
  where
    resolve h =
      case resolveAssetId mapping (holdingRawId h) of
        Just cid -> h { holdingCanonicalId = Just cid }
        Nothing  -> h { holdingCanonicalId = Just (CanonicalAssetId (unRawAssetId (holdingRawId h))) }

