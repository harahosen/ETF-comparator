module Parser.IShares
  ( parseIShares
  ) where

import Domain.Types (RawETF, FundId)
import Parser.Raw (parseRawTable)
import Data.Text (Text)

type Table = [[Text]]

parseIShares :: FundId -> Table -> Either String RawETF
parseIShares = parseRawTable
