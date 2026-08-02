module Ingestion.UnsolvedId
  ( reportUnsolved
  ) where

import Domain.Types
import Control.Monad (unless)

reportUnsolved :: RawFundId -> [RawAssetId] -> IO ()
reportUnsolved fundId ids =
  unless (null ids) $
    putStrLn $
      "Unresolved asset ids for fund " <> show fundId <> ":\n"
      <> unlines (map show ids)
