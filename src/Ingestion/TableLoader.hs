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
  -- For XLSX, we'd need a library like 'xlsx' package
  -- For now, return an error or suggest adding the dependency
  error $ "XLSX loading not implemented. Add 'xlsx' package to dependencies."