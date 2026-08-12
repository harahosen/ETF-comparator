module Domain.Normalization
  ( normalizeETF
  , normalizeETFWithTolerance
  , isNormalized
  ) where

import Domain.Types
import Domain.Errors

import qualified Data.Map.Strict as M

-- tolerance for floating-point comparisons
epsilon :: Double
epsilon = 1e-6

-- check if an ETF is already normalized
isNormalized :: RawETF -> Bool
isNormalized (RawETF _ hs) =
  abs (totalWeight hs - 1.0) < epsilon

-- ETF normalization (total holdings weight = 1)
normalizeETF :: RawETF -> Either NormalizationError NormalizedETF
normalizeETF = normalizeETFWithTolerance epsilon

-- ETF normalization with custom tolerance
normalizeETFWithTolerance :: Double -> RawETF -> Either NormalizationError NormalizedETF
normalizeETFWithTolerance tolerance (RawETF _ hs)
  | total <= 0 = Left ZeroTotalWeight
  | abs (total - 1.0) <= tolerance =
      Right $ NormalizedETF (M.fromList pairs)
  | otherwise =
      Right $ NormalizedETF
        (M.fromList
          [ (cid, Weight (unWeight w / total))
          | (cid, w) <- pairs
          ])
  where
    pairs =
      [ (cid, holdingWeight h)
      | h <- hs
      , Just cid <- [holdingCanonicalId h]
      ]
    total = sum (map (unWeight . snd) pairs)

-- sum of all weights
totalWeight :: [Holding] -> Double
totalWeight =
  sum . map (unWeight . holdingWeight)
