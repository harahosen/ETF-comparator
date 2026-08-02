module Ingestion.Router
  ( route
  ) where

import Ingestion.FileMeta
import Parser.IShares
import Parser.StateStreet
import Parser.Custom
import Parser.Preprocess.IShares
import Parser.Preprocess.StateStreet
import Parser.Preprocess.Custom
import Domain.Types (RawETF)
import Data.Text (Text)

type Table = [[Text]]
type Preprocessor = Table -> Either String Table
type Parser = Table -> Either String RawETF

route :: FileMeta -> (Preprocessor, Parser)
route meta =
  case (fmProvider meta, fmFormat meta) of
    (IS, CSV)   -> (preprocessIShares, parseIShares)
    (SS, XLSX)  -> (preprocessStateStreet, parseStateStreet)
    (CF, CSV)   -> (preprocessCustom, parseCustom)
    _           -> error "Unsupported provider/format"
