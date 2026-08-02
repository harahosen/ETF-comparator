module Ingestion.TableLoader
  ( loadTable
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Char8 as BC
import Ingestion.FileMeta (FileMeta(..), FileFormat(..))
import Data.Csv (decode, HasHeader(..))
import qualified Data.Vector as V
import qualified Codec.Xlsx as XLSX
import Codec.Xlsx.Lens (ixSheet, cellValueAt)
import Control.Lens ((^?), (^..), (^.))

type Table = [[Text]]

loadTable :: FileMeta -> IO Table
loadTable meta = case fmFormat meta of
  CSV -> loadCSV (fmPath meta)
  XLSX -> loadXLSX (fmPath meta)

loadCSV :: FilePath -> IO Table
loadCSV path = do
  bs <- LBS.readFile path
  let result = decode HasHeader bs
  case result of
    Left err -> error $ "CSV parsing error: " ++ err
    Right records -> do
      let recordList = V.toList records
          dataRows = [map (T.pack . BC.unpack) (V.toList record) | record <- recordList]
      return dataRows

loadXLSX :: FilePath -> IO Table
loadXLSX path = do
  bs <- LBS.readFile path
  let xlsx = XLSX.toXlsx bs
  case xlsx ^? ixSheet "Sheet1" of
    Nothing -> error "XLSX file has no sheet named 'Sheet1'"
    Just sheet -> return $ sheetToTable sheet
  where
    sheetToTable sheet =
      let maxRow = 100  -- reasonable default
          maxCol = 100  -- reasonable default
          table = [[cellToText row col sheet | col <- [0..maxCol]] | row <- [0..maxRow]]
      in table

    cellToText row col sheet =
      case sheet ^. cellValueAt (row, col) of
        Just (XLSX.CellText t) -> t
        Just (XLSX.CellDouble d) -> T.pack (show d)
        Just (XLSX.CellBool b) -> T.pack (show b)
        Nothing -> T.empty