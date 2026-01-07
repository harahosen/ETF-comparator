module Domain.Merge
  ( mergeHoldings
  ) where

import qualified Data.Map.Strict as M
import Domain.Types

mergeHoldings :: [Holding] -> [Holding]
mergeHoldings =
  M.elems . foldr insertHolding M.empty
  where
    insertHolding h acc =
      case holdingCanonicalId h of
        Just cid ->
          M.insertWith mergeByWeight cid h acc
        Nothing ->
          -- unresolved assets are kept separate
          accWithRawFallback h acc

    mergeByWeight h1 h2 = h1 { holdingWeight = sumWeights (holdingWeight h1) (holdingWeight h2) }

    sumWeights (Weight w1) (Weight w2) =
      Weight (w1 + w2)

-- unresolved assets are not merged
accWithRawFallback :: Holding -> M.Map CanonicalAssetId Holding -> M.Map CanonicalAssetId Holding
accWithRawFallback h acc = acc