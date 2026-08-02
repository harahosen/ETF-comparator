module Ingestion.ResolveFundId
  ( resolveETFFundId
  ) where

import Domain.Types
import Ingestion.FundIdMapping

resolveETFFundId :: FundIdMapping -> RawETF -> Either RawFundId RawETF
resolveETFFundId mapping (RawETF rawFundId _ hs) =
  case resolveFundId mapping rawFundId of
    Nothing -> Left rawFundId
    Just canonicalFundId -> Right (RawETF rawFundId (Just canonicalFundId) hs)
