module Ingestion.ResolveAssetId
  ( resolveETFAssetIds
  ) where

import Domain.Types
import Ingestion.AssetIdMapping

resolveETFAssetIds :: AssetIdMapping -> RawETF -> (RawETF, [RawAssetId])
resolveETFAssetIds mapping (RawETF fid hs) =
  (RawETF fid resolved, unresolved)
  where
    (resolved, unresolved) =
      partitionEithers (map resolve hs)
    resolve h =
      case resolveAssetId mapping (holdingRawId h) of
        Just cid -> Left h { holdingCanonicalId = Just cid }
        Nothing  -> Right (holdingRawId h)

