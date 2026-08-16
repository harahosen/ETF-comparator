module Ingestion.TableLoader
  ( loadTable
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Char8 as BC
import Ingestion.FileMeta (FileMeta(..), FileFormat(..))
import Ingestion.Xlsx (loadXlsx)
import Data.Csv (decode, HasHeader(..))
import qualified Data.Vector as V

type Table = [[Text]]

loadTable :: FileMeta -> IO Table
loadTable meta =
  case fmFormat meta of
    CSV  -> loadCSV (fmPath meta)
    XLSX -> loadXlsx (fmPath meta)

loadCSV :: FilePath -> IO Table
loadCSV path = do
  bs <- LBS.readFile path
  let result = decode NoHeader bs
  case result of
    Left err -> error $ "CSV parsing error: " ++ err
    Right records -> do
      let recordList = V.toList records
          dataRows = [map (T.pack . BC.unpack) (V.toList record) | record <- recordList]
      return dataRows
