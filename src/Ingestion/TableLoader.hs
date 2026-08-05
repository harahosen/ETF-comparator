module Ingestion.TableLoader
  ( loadTable
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Char8 as BC
import Ingestion.FileMeta (FileMeta(..))
import Data.Csv (decode, HasHeader(..))
import qualified Data.Vector as V

type Table = [[Text]]

loadTable :: FileMeta -> IO Table
loadTable = loadCSV . fmPath

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
