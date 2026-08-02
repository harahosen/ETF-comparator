module Ingestion.ResolveAssetId
  ( resolveETFAssetIds
  ) where

import Domain.Types
import Ingestion.AssetIdMapping
import Data.Either (partitionEithers)

resolveETFAssetIds :: AssetIdMapping -> RawETF -> (RawETF, [RawAssetId])
resolveETFAssetIds mapping (RawETF rawFundId maybeCanonicalFundId hs) =
  (RawETF rawFundId maybeCanonicalFundId resolved, unresolved)
  where
    (resolved, unresolved) =
      partitionEithers (map resolve hs)
    resolve h =
      case resolveAssetId mapping (holdingRawId h) of
        Just cid -> Left h { holdingCanonicalId = Just cid }
        Nothing  -> Right (holdingRawId h)

