module Ingestion.Loader
  ( loadETF
  ) where

import Ingestion.Router
import Ingestion.FileMeta

loadETF :: FilePath -> IO (Either String RawETF)
loadETF path = do
  meta   <- parseFileMeta path
  table  <- loadTable path
  let (pre, parse) = route meta
  pure $ do
    cleaned <- pre table
    parse cleaned
