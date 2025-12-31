module Ingestion.Loader
  ( loadETF
  ) where

import Ingestion.FileMeta
import Ingestion.Router
import Domain.Types

-- load of an ETF composition from a file path
loadETF :: FilePath -> IO (Either String RawETF)
loadETF path =
  case inferFileMeta path of
    Left err -> pure (Left err)
    Right meta -> Right <$> routeETF meta
