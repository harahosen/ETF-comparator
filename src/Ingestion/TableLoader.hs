module Ingestion.TableLoader
  ( loadTable
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.ByteString as BS
import Ingestion.FileMeta (FileMeta(..), FileFormat(..))
import Data.Csv (decodeByName)
import qualified Data.Vector as V
import Codec.Xlsx
import qualified Data.ByteString.Lazy as LBS

type Table = [[Text]]

loadTable :: FileMeta -> IO Table
loadTable meta = case fmFormat meta of
  CSV -> loadCSV (fmPath meta)
  XLSX -> loadXLSX (fmPath meta)

loadCSV :: FilePath -> IO Table
loadCSV path = do
  result <- decodeByName <$> BS.readFile path
  case result of
    Left err -> error $ "CSV parsing error: " ++ err
    Right (header, records) -> do
      let headerRow = map T.pack (V.toList header)
          dataRows = [map T.pack (V.toList record) | record <- V.toList records]
      return (headerRow : dataRows)

loadXLSX :: FilePath -> IO Table
loadXLSX path = do
  bs <- LBS.readFile path
  let xlsx = fromXlsx bs
  case toXlsx xlsx of
    [] -> error "XLSX file has no sheets"
    (sheetName, sheet) : _ -> 
      let table = sheetToTable sheet
      in return table
  where
    sheetToTable sheet = 
      let cellMap = toCells sheet
          rowIndices = [row | (row, _, _) <- cellMap]
          colIndices = [col | (_, col, _) <- cellMap]
          maxRow = if null rowIndices then 0 else maximum rowIndices
          maxCol = if null colIndices then 0 else maximum colIndices
          table = [[cellToText (row, col) cellMap | col <- [0..maxCol]] | row <- [0..maxRow]]
      in table
    
    cellToText (row, col) cellMap = 
      case lookup (row, col) cellMap of
        Just cell -> renderCell cell
        Nothing   -> T.empty
    
    renderCell cell = 
      case cellValue cell of
        Just (CellText t) -> t
        Just (CellDouble d) -> T.pack (show d)
        Just (CellBool b) -> T.pack (show b)
        Nothing -> T.empty