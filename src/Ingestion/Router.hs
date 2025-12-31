module Ingestion.Router
  ( routeETF
  ) where

import Ingestion.FileMeta
import Domain.Types
import qualified Parser.IShares as IS
import qualified Parser.StateStreet as SS

-- parser selection based on vendor and format.
routeETF :: FileMeta -> IO RawETF
routeETF meta =
  case (fmVendor meta, fmFormat meta) of
    (IShares, CSV) ->
      IS.parseISharesCSV (fmPath meta)

    (StateStreet, XLSX) ->
      SS.parseStateStreetXLSX (fmPath meta)
