module Ingestion.Router
  ( route
  ) where

import Ingestion.FileMeta
import Parser.IShares
import Parser.StateStreet
import Parser.Custom

route
  :: FileMeta
  -> ([[Text]] -> Either String [[Text]])  -- preprocessor
  -> ([[Text]] -> Either String RawETF)     -- parser
route meta =
  case (provider meta, format meta) of
    (IS, CSV)   -> (preprocessIShares, parseIShares fundId)
    (SS, XLSX)  -> (preprocessStateStreet, parseStateStreet fundId)
    (CF, CSV)   -> (preprocessCustom, parseCustom fundId)
    _           -> error "Unsupported provider/format"
