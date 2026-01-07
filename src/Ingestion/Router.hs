module Ingestion.Router
  ( routeETF
  ) where

import Ingestion.FileMeta
import Domain.Types

import qualified Parser.IShares as IS
import qualified Parser.StateStreet as SS
import qualified Parser.Custom as CF

routeETF :: FileMeta -> IO RawETF
routeETF meta =
  case (fmProvider meta, fmFormat meta) of

    (IS, CSV) ->
      IS.parseISharesCSV (fmPath meta)

    (SS, XLSX) ->
      SS.parseStateStreetXLSX (fmPath meta)

    (CF, CSV) ->
      CF.parseCustomCSV (fmPath meta)

    (provider, format) ->
      fail $
        "Unsupported provider/format combination: "
        <> show provider <> " / " <> show format
