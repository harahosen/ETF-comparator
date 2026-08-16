{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Domain.Types
  ( FundId(..)
  , RawAssetId(..)
  , CanonicalAssetId(..)
  , Weight(..)
  , Holding(..)
  , RawETF(..)
  , NormalizedETF(..)
  ) where

import Data.Map.Strict (Map)
import Data.Text (Text)

-- vendor-agnostic ETF identifier (ISIN, tiket, code, name... whatever)
newtype FundId = FundId { unFundId :: String }
  deriving (Eq, Ord, Show)

-- vendor-specific asset identifier
newtype RawAssetId = RawAssetId  { unRawAssetId :: String }
  deriving (Eq, Ord, Show)

-- vendor-agnostic asset identifier
newtype CanonicalAssetId = CanonicalAssetId  { unCanonicalAssetId :: String }
  deriving (Eq, Ord, Show)

-- holding weight
newtype Weight = Weight { unWeight :: Double }
  deriving (Eq, Ord, Show, Num)

instance Semigroup Weight where
  (<>) = (+)

instance Monoid Weight where
  mempty = 0

-- general holding definition
data Holding = Holding
  { holdingRawId        :: RawAssetId
  , holdingCanonicalId  :: Maybe CanonicalAssetId
  , holdingName         :: Maybe Text
  , holdingWeight       :: Weight
  } deriving (Eq, Show)

-- ETF parsed at it is
data RawETF = RawETF
  { etfFundId   :: FundId
  , etfHoldings :: [Holding]
  } deriving (Eq, Show)

-- ETF after validation and normalization:
-- 1. unique assetId
-- 2. weights sum to 1 (within tolerance)
-- 3. weights are non-negative and finite
newtype NormalizedETF =
  NormalizedETF (Map CanonicalAssetId Weight)
  deriving (Eq, Show)
