-- Parser/Preprocess/StateStreet.hs
module Parser.Preprocess.StateStreet
  ( preprocessStateStreet
  ) where

import Parser.Common.Table
import Parser.Common.PreprocessHelpers

isEmptyRow :: Row -> Bool
isEmptyRow = all null

preprocessStateStreet :: Table -> Either String Table
preprocessStateStreet rows = do
  (headerIndex, header) <- findHeader rows

  let colCount = length header
      candidateRows =
        filter (not . isEmptyRow)
        . drop (headerIndex + 1)
        $ rows

      body =
        takeWhile (isDataRow colCount) candidateRows

  if null body
     then Left "StateStreet: no holdings rows found"
     else Right (header : body)
