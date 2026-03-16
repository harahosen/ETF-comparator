module Ingestion.TableLoader
  ( loadTable
  ) where

import Data.Text (Text)

type Table = [[Text]]

loadTable :: FilePath -> IO Table
loadTable _ = pure []