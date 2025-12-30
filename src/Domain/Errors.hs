module Domain.Errors
  ( ValidationError(..)
  , NormalizationError(..)
  , ComparisonError(..)
  , DomainError(..)
  ) where

import Domain.Types (AssetId)

-- errors for malformed or inconsistent input.
data ValidationError
  = EmptyHoldings
  | NegativeWeight AssetId
  | DuplicateHolding AssetId
  | NonFiniteWeight AssetId
  deriving (Eq, Show)

-- errors for mathematically inconsistent compositions
data NormalizationError
  = ZeroTotalWeight
  | InvalidTotalWeight Double
  deriving (Eq, Show)

{-
-- errors not expected at the moment, commented or now
data ComparisonError
  = IncompatibleUniverse
  deriving (Eq, Show)
-}

-- pattern-matching on error origin for higher layers
data DomainError
  = ValidationErr ValidationError
  | NormalizationErr NormalizationError
  deriving (Eq, Show)
--  | ComparisonErr ComparisonError
  
