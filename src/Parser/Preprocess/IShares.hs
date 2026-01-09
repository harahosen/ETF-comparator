module Parser.Preprocess.IShares
  ( preprocessIShares
  ) where

import Parser.Common.PreprocessHelpers
import Data.Text (Text)

type Table = [[Text]]

preprocessIShares :: Table -> Either String Table
preprocessIShares table =
  case trimTable (dropEmptyRows table) of
    [] -> Left "IShares preprocess: empty table after cleaning"
    t -> Right t
