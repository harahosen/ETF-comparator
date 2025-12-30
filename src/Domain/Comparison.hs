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

-- cosine similarity between two normalized ETFs
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

-- cosine distance between two normalized ETFs
cosineDistance :: NormalizedETF -> NormalizedETF -> Double
cosineDistance a b =
  1.0 - cosineSimilarity a b

-- weighted Jaccard similarity betwen normalied ETFs
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

-- weighted Jaccard distance between two normalized ETFs
weightedJaccardDistance :: NormalizedETF -> NormalizedETF -> Double
weightedJaccardDistance a b =
  1.0 - weightedJaccardSimilarity a b

-- overlap ratio between tow normalized ETFs
-- ("how much the smaller ETF is contained in the larger one")
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
