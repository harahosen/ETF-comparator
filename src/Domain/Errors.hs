module Domain.Errors
  ( ValidationError(..)
  , NormalizationError(..)
  , ComparisonError(..)
  , DomainError(..)
  ) where


import Domain.Types (CanonicalAssetId)

-- errors for malformed or inconsistent input.
data ValidationError
  = EmptyHoldings
  | NegativeWeight CanonicalAssetId
  | DuplicateHolding CanonicalAssetId
  | NonFiniteWeight CanonicalAssetId
  deriving (Eq, Show)

-- errors for mathematically inconsistent compositions
data NormalizationError
  = ZeroTotalWeight
  | InvalidTotalWeight Double
  deriving (Eq, Show)

data ComparisonError
  = WeightMismatch CanonicalAssetId
  | MissingAsset CanonicalAssetId
  deriving (Eq, Show)

-- pattern-matching on error origin for higher layers
data DomainError
  = ValidationErr ValidationError
  | NormalizationErr NormalizationError
  deriving (Eq, Show)
--  | ComparisonErr ComparisonError
  
