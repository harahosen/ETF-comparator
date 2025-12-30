module Domain.Comparison
  ( cosineSimilarity
  , cosineDistance
  , weightedJaccardSimilarity
  , weightedJaccardDistance
  , overlapRatio
  ) where

import Domain.Types

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- | Cosine similarity between two normalized ETFs.
--   Measures similarity of weight distributions.
cosineSimilarity :: NormalizedETF -> NormalizedETF -> Double
cosineSimilarity (NormalizedETF a) (NormalizedETF b) =
  dot / (norm a * norm b)
  where
    dot =
      sum
        [ unWeight wa * unWeight wb
        | (k, wa) <- Map.toList a
        , Just wb <- [Map.lookup k b]
        ]

    norm m =
      sqrt . sum $ map (\(Weight w) -> w * w) (Map.elems m)

-- | Cosine distance is defined as 1 - cosine similarity.
cosineDistance :: NormalizedETF -> NormalizedETF -> Double
cosineDistance a b =
  1.0 - cosineSimilarity a b

-- | Weighted Jaccard similarity.
--   Captures overlap magnitude rather than direction.
weightedJaccardSimilarity :: NormalizedETF -> NormalizedETF -> Double
weightedJaccardSimilarity (NormalizedETF a) (NormalizedETF b) =
  intersection / union
  where
    keys = Set.union (Map.keysSet a) (Map.keysSet b)

    intersection =
      sum
        [ min wa wb
        | k <- Set.toList keys
        , let wa = maybe 0 unWeight (Map.lookup k a)
        , let wb = maybe 0 unWeight (Map.lookup k b)
        ]

    union =
      sum
        [ max wa wb
        | k <- Set.toList keys
        , let wa = maybe 0 unWeight (Map.lookup k a)
        , let wb = maybe 0 unWeight (Map.lookup k b)
        ]

-- | Complement of weighted Jaccard similarity.
weightedJaccardDistance :: NormalizedETF -> NormalizedETF -> Double
weightedJaccardDistance a b =
  1.0 - weightedJaccardSimilarity a b

-- | Overlap ratio measures how much the smaller ETF
--   is contained in the larger one.
overlapRatio :: NormalizedETF -> NormalizedETF -> Double
overlapRatio (NormalizedETF a) (NormalizedETF b) =
  intersection / min totalA totalB
  where
    intersection =
      sum
        [ min wa wb
        | (k, Weight wa) <- Map.toList a
        , Just (Weight wb) <- [Map.lookup k b]
        ]

    totalA = sum (map unWeight (Map.elems a))
    totalB = sum (map unWeight (Map.elems b))
