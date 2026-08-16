module Domain.Merge
  ( mergeResolvedHoldings
  ) where

import Domain.Types
import qualified Data.Map.Strict as M
import Data.List (partition)
import Data.Maybe (isJust)

-- | Merge holdings that have a canonical asset id.
--   Holdings without canonical ids are left untouched.
mergeResolvedHoldings :: RawETF -> RawETF
mergeResolvedHoldings (RawETF fundId holdings) =
  RawETF fundId (merged ++ unresolved)
  where
    (resolved, unresolved) = partition hasCanonicalId holdings

    merged =
      M.elems $
        M.fromListWith mergeHolding
          [ (cid, h)
          | h@(Holding _ (Just cid) _ _) <- resolved
          ]

    mergeHolding h1 h2 =
      h1 { holdingWeight = holdingWeight h1 <> holdingWeight h2 }

hasCanonicalId :: Holding -> Bool
hasCanonicalId = isJust . holdingCanonicalId