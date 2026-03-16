module Domain.Comparison
  ( cosineSimilarity
  , cosineDistance
  , weightedJaccardSimilarity
  , weightedJaccardDistance
  , overlapRatio
  ) where

import Domain.Types

import qualified Data.Map.Strict as M
import qualified Data.Set as Set

-- cosine similarity between two normalized ETFs
cosineSimilarity :: NormalizedETF -> NormalizedETF -> Double
cosineSimilarity (NormalizedETF a) (NormalizedETF b) =
  dot / (norm a * norm b)
  where
    dot =
      sum
        [ unWeight wa * unWeight wb
        | (k, wa) <- M.toList a
        , Just wb <- [M.lookup k b]
        ]

    norm m =
      sqrt . sum $ map (\(Weight w) -> w * w) (M.elems m)

-- cosine distance between two normalized ETFs
cosineDistance :: NormalizedETF -> NormalizedETF -> Double
cosineDistance a b =
  1.0 - cosineSimilarity a b

-- weighted Jaccard similarity betwen normalied ETFs
weightedJaccardSimilarity :: NormalizedETF -> NormalizedETF -> Double
weightedJaccardSimilarity (NormalizedETF a) (NormalizedETF b) =
  intersection / union
  where
    keys = Set.union (M.keysSet a) (M.keysSet b)

    intersection =
      sum
        [ min wa wb
        | k <- Set.toList keys
        , let wa = maybe 0 unWeight (M.lookup k a)
        , let wb = maybe 0 unWeight (M.lookup k b)
        ]

    union =
      sum
        [ max wa wb
        | k <- Set.toList keys
        , let wa = maybe 0 unWeight (M.lookup k a)
        , let wb = maybe 0 unWeight (M.lookup k b)
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
        | (k, Weight wa) <- M.toList a
        , Just (Weight wb) <- [M.lookup k b]
        ]

    totalA = sum (map unWeight (M.elems a))
    totalB = sum (map unWeight (M.elems b))
