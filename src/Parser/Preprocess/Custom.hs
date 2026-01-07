-- Parser/Preprocess/Custom.hs
module Parser.Preprocess.Custom
  ( preprocessCustom
  ) where

import Parser.Common.Table
import Parser.Common.PreprocessHelpers

dropEmptyRows :: Table -> Table
dropEmptyRows = filter (not . all null)

preprocessCustom :: Table -> Either String Table
preprocessCustom rows = do
  let cleaned = dropEmptyRows rows

  case cleaned of
    [] -> Left "Custom: empty file"
    (header : body) ->
      if isHeaderRow header
         then Right (header : body)
         else Left "Custom: invalid header row"
