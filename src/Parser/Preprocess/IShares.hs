-- Parser/Preprocess/IShares.hs
module Parser.Preprocess.IShares
  ( preprocessIShares
  ) where

import Parser.Common.Table
import Parser.Preprocess.Common

preprocessIShares :: Table -> Either String Table
preprocessIShares rows = do
  (headerIndex, header) <- findHeader rows

  let colCount = length header
      body =
        takeWhile (isDataRow colCount)
        . drop (headerIndex + 1)
        $ rows

  if null body
     then Left "IShares: no holdings rows found"
     else Right (header : body)
