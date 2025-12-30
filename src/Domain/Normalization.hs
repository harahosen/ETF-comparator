module Domain.Normalization
  ( normalizeETF
  , isNormalized
  ) where

import Domain.Types
import Domain.Errors

import qualified Data.Map.Strict as Map

-- | Numerical tolerance for floating-point comparisons.
epsilon :: Double
epsilon = 1e-6

-- | Checks whether a RawETF is already normalized.
--   This is useful for diagnostics and short-circuiting.
isNormalized :: RawETF -> Bool
isNormalized (RawETF _ hs) =
  abs (totalWeight hs - 1.0) < epsilon

-- | Normalizes ETF holdings so that weights sum to 1.
--   Converts the representation to NormalizedETF,
--   enforcing stronger invariants via Map.
normalizeETF :: RawETF -> Either NormalizationError NormalizedETF
normalizeETF (RawETF _ hs)
  | total <= 0 = Left ZeroTotalWeight
  | otherwise =
      Right $
        NormalizedETF $
          Map.fromList
            [ (holdingAssetId h, Weight (unWeight (holdingWeight h) / total))
            | h <- hs
            ]
  where
    total = totalWeight hs

-- | Computes the sum of all weights.
totalWeight :: [Holding] -> Double
totalWeight =
  sum . map (unWeight . holdingWeight)
