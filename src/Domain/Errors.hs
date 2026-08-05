module Domain.Errors
  ( ValidationError(..)
  , NormalizationError(..)
  , ComparisonError(..)
  , DomainError(..)
  , PipelineError(..)
  ) where


import Domain.Types (CanonicalAssetId, unCanonicalAssetId)
import Data.Aeson (ToJSON(..), object, (.=))
import Data.Text (Text)

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
  | UnresolvedHoldings Int  -- Number of holdings without canonical IDs
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

-- | Normalized error representation produced by the pipeline for output.
data PipelineError
  = ValidationPE ValidationError
  | NormalizationPE NormalizationError
  | LoadPE String
  deriving (Eq)

instance Show PipelineError where
  show (ValidationPE e)   = show e
  show (NormalizationPE e) = show e
  show (LoadPE s)         = s

instance ToJSON PipelineError where
  toJSON (ValidationPE e) = case e of
    EmptyHoldings ->
      object ["type" .= ("EmptyHoldings" :: Text)]
    NegativeWeight c ->
      object ["type" .= ("NegativeWeight" :: Text), "assetId" .= unCanonicalAssetId c]
    DuplicateHolding c ->
      object ["type" .= ("DuplicateHolding" :: Text), "assetId" .= unCanonicalAssetId c]
    NonFiniteWeight c ->
      object ["type" .= ("NonFiniteWeight" :: Text), "assetId" .= unCanonicalAssetId c]

  toJSON (NormalizationPE e) = case e of
    ZeroTotalWeight ->
      object ["type" .= ("ZeroTotalWeight" :: Text)]
    InvalidTotalWeight d ->
      object ["type" .= ("InvalidTotalWeight" :: Text), "total" .= d]
    UnresolvedHoldings n ->
      object ["type" .= ("UnresolvedHoldings" :: Text), "count" .= n]

  toJSON (LoadPE s) =
    object ["type" .= ("Load" :: Text), "message" .= s]
