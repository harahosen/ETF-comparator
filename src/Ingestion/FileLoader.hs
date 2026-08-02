module Ingestion.FileLoader
  ( loadETF
  ) where

import Ingestion.Router
import Ingestion.FileMeta
import Ingestion.TableLoader
import Domain.Types
import Control.Monad.Except
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)

type Table = [[Text]]

loadETF :: FilePath -> IO (Either String RawETF)
loadETF path = runExceptT $ do
  meta <- ExceptT (pure (deriveFileMetadata path))
  table <- liftIO (loadTable path)
  let fundId = FundId (fmDate meta)
      (pre, parse) = route fundId meta
  cleaned <- ExceptT (pure (pre table))
  ExceptT (pure (parse cleaned))