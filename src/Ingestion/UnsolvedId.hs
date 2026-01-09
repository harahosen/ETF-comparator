module Ingestion.UnsolvedId
  ( reportUnsolved
  ) where

import Domain.Types

reportUnsolved :: FundId -> [RawAssetId] -> IO ()
reportUnsolved fundId ids =
  unless (null ids) $
    putStrLn $
      "Unresolved asset ids for " <> show fundId <> ":\n"
      <> unlines (map show ids)
