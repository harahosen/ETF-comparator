module Domain.Normalization
  ( normalizeETF
  , normalizeETFWithTolerance
  , isNormalized
  ) where

import Domain.Types
import Domain.Errors

import qualified Data.Map.Strict as M
import Data.Maybe (isJust)

-- tolerance for floating-point comparisons
epsilon :: Double
epsilon = 1e-6

-- check if an ETF is already normalized
isNormalized :: RawETF -> Bool
isNormalized (RawETF _ _ hs) =
  abs (totalWeight hs - 1.0) < epsilon

-- ETF normalization (total holdings weight = 1)
normalizeETF :: RawETF -> Either NormalizationError NormalizedETF
normalizeETF = normalizeETFWithTolerance epsilon

-- ETF normalization with custom tolerance
normalizeETFWithTolerance :: Double -> RawETF -> Either NormalizationError NormalizedETF
normalizeETFWithTolerance tolerance (RawETF _ maybeCanonicalFundId hs)
  | originalTotal <= 0 = Left ZeroTotalWeight
  | otherwise =
      case maybeCanonicalFundId of
        Nothing -> Left MissingCanonicalFundId
        Just canonicalFundId ->
          let resolvedOnly = [h | h <- hs, isJust (holdingCanonicalId h)]
              unresolvedCount = length hs - length resolvedOnly
              resolvedTotal = totalWeight resolvedOnly
              resolvedPairs = [(cid, holdingWeight h) | h <- resolvedOnly, Just cid <- [holdingCanonicalId h]]
          in if null resolvedOnly
             then Left (UnresolvedHoldings unresolvedCount)
             else if abs (resolvedTotal - 1.0) <= tolerance
               then Right $ NormalizedETF
                    { normalizedFundId = canonicalFundId
                    , normalizedAssets = M.fromList resolvedPairs
                    }
               else Right $ NormalizedETF
                    { normalizedFundId = canonicalFundId
                    , normalizedAssets = M.fromList
                        [ (cid, Weight (unWeight weight / resolvedTotal))
                        | (cid, weight) <- resolvedPairs
                        ]
                    }
  where
    originalTotal = totalWeight hs

-- sum of all weights
totalWeight :: [Holding] -> Double
totalWeight =
  sum . map (unWeight . holdingWeight)

{-
The following functions are not needed with the current data flow
keeping them for now because they could be useful if an already normalized ETF have to be checked
(it should not be possible by design, but with new sources something could change a little)

weightsSumToOne :: Foldable f => f Weight -> Bool
weightsSumToOne ws =
  abs (sum (map unWeight (toList ws)) - 1.0) < epsilon

isNormalizedRaw :: RawETF -> Bool
isNormalizedRaw (RawETF _ hs) =
  weightsSumToOne (map holdingWeight hs)

assertNormalized :: NormalizedETF -> Either NormalizationError ()
assertNormalized (NormalizedETF m)
  | weightsSumToOne (M.elems m) = Right ()
  | otherwise = Left InvalidNormalization
-}