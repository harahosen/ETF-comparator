module Parser.Preprocess.StateStreet
  ( preprocessStateStreet
  ) where

import Parser.Common.PreprocessHelpers
import Data.Text (Text)

type Table = [[Text]]

preprocessStateStreet :: Table -> Either String Table
preprocessStateStreet table =
  case trimTable (dropEmptyRows table) of
    [] -> Left "StateStreet preprocess: empty table after cleaning"
    t  -> Right t
