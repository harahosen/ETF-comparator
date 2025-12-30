module Domain.Types
  ( FundId(..)
  , AssetId(..)
  , Weight(..)
  , Holding(..)
  , RawETF(..)
  , NormalizedETF(..)
  ) where

import Data.Map.Strict (Map)

-- vendor-agnostic ETF identifier (ISIN, tiket, code, name... whatever)
newtype FundId = FundId { unFundId :: String }
  deriving (Eq, Ord, Show)

-- vendor-agnostic asset identifier
newtype AssetId = AssetId { unAssetId :: String }
  deriving (Eq, Ord, Show)

-- holding weight
newtype Weight = Weight { unWeight :: Double }
  deriving (Eq, Ord, Show)

-- general holding definition
data Holding = Holding
  { holdingAssetId :: AssetId
  , holdingWeight  :: Weight
  } deriving (Eq, Show)

-- ETF parsed at it is
data RawETF = RawETF
  { etfFundId   :: FundId
  , etfHoldings :: [Holding]
  } deriving (Eq, Show)

-- ETF after validation and normalization:
-- 1. unique AssetId
-- 2. weights sum to 1 (within tolerance)
-- 3. weights are non-negative and finite
newtype NormalizedETF =
  NormalizedETF (Map AssetId Weight)
  deriving (Eq, Show)
