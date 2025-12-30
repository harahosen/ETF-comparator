module Domain.Normalization
  ( normalizeETF
  , isNormalized
  ) where

import Domain.Types
import Domain.Errors

import qualified Data.Map.Strict as Map

-- tolerance for floating-point comparisons
epsilon :: Double
epsilon = 1e-6

-- check if an ETF is already normalized
isNormalized :: RawETF -> Bool
isNormalized (RawETF _ hs) =
  abs (totalWeight hs - 1.0) < epsilon

-- ETF normalization (total holdings weight = 1)
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
  | weightsSumToOne (Map.elems m) = Right ()
  | otherwise = Left InvalidNormalization
-}