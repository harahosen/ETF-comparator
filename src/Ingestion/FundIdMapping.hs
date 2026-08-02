module Ingestion.FundIdMapping
  ( FundIdMapping
  , resolveFundId
  ) where

import qualified Data.Map.Strict as M
import Domain.Types

type FundIdMapping = M.Map RawFundId CanonicalFundId

-- | Resolves raw fund identifiers into canonical ids
--   Resolution order is defined by mapping construction.
resolveFundId :: FundIdMapping -> RawFundId -> Maybe CanonicalFundId
resolveFundId m rawFundId = M.lookup rawFundId m
