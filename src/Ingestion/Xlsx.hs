module Ingestion.Xlsx
  ( loadXlsx
  ) where

import Codec.Xlsx
import Control.Lens (view)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.Text (Text)

loadXlsx :: FilePath -> IO [[Text]]
loadXlsx path = do
  bs <- LBS.readFile path
  let xlsx = toXlsx bs
      sheets = view xlSheets xlsx
  case map snd sheets of
    []    -> return []
    (s:_) -> do
      let rows = toRows (view wsCells s)
          maxCol = maximum $ 0 : [ c | (_, cells) <- rows, (c, _) <- cells ]
      return $ map (rowToTexts maxCol) rows
  where
    rowToTexts maxCol (_, cells) =
      let cellMap = M.fromList cells
      in [ cellText (M.lookup c cellMap) | c <- [1..maxCol] ]

    cellText :: Maybe Cell -> Text
    cellText Nothing = ""
    cellText (Just cell) =
      case view cellValue cell of
        Nothing -> ""
        Just (CellText t)   -> t
        Just (CellDouble d) -> T.pack (show d)
        Just (CellBool b)   -> T.pack (show b)
        Just (CellRich _)   -> ""
        Just (CellError _)  -> ""
