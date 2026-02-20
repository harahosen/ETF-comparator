module Ingestion.Router
  ( route
  ) where

import Ingestion.FileMeta
import Parser.IShares
import Parser.StateStreet
import Parser.Custom
import Domain.Types (RawETF)
import Data.Text (Text)

route
  :: FoundId -> FileMeta
  -> ([[Text]] -> Either String [[Text]])  -- preprocessor
  -> ([[Text]] -> Either String RawETF)     -- parser
route meta =
  case (fmProvider meta, fmFormat meta) of
    (IS, CSV)   -> (preprocessIShares, parseIShares fundId)
    (SS, XLSX)  -> (preprocessStateStreet, parseStateStreet fundId)
    (CF, CSV)   -> (preprocessCustom, parseCustom fundId)
    _           -> error "Unsupported provider/format"
