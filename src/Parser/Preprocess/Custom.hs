module Parser.Preprocess.Custom
  ( preprocessCustom
  ) where

import Parser.Common.PreprocessHelpers
import Data.Text (Text)

type Table = [[Text]]

preprocessCustom :: Table -> Either String Table
preprocessCustom =
  Right . dropEmptyRows
