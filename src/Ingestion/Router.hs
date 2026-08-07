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
import Domain.Types (RawETF, FundId)
import Data.Text (Text)

type Table = [[Text]]
type Preprocessor = Table -> Either String Table
type Parser = Table -> Either String RawETF

route :: FundId -> FileMeta -> (Preprocessor, Parser)
route fundId meta =
  case (fmProvider meta, fmFormat meta) of
    (IS, CSV)   -> (preprocessIShares, parseIShares fundId)
    (SS, CSV)   -> (preprocessStateStreet, parseStateStreet fundId)
    (CF, CSV)   -> (preprocessCustom, parseCustom fundId)
    _           -> (\_ -> Left "XLSX is not supported directly by etf-comparator; use reconcile to adapt it first"
                   ,\_ -> Left "XLSX is not supported directly by etf-comparator")
