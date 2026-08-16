module Parser.StateStreet
  ( parseStateStreet
  ) where

import Domain.Types (RawETF, FundId)
import Parser.Raw (parseRawTable)
import Data.Text (Text)

type Table = [[Text]]

parseStateStreet :: FundId -> Table -> Either String RawETF
parseStateStreet = parseRawTable
