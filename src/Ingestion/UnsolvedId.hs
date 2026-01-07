module Ingestion.UnsolvedId
  ( UnresolvedReport
  , emptyReport
  , recordUnresolved
  , writeReport
  ) where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Domain.Types

type UnresolvedReport =
  M.Map FundId (S.Set RawAssetId)

emptyReport :: UnresolvedReport
emptyReport = M.empty

recordUnresolved
  :: FundId
  -> RawAssetId
  -> UnresolvedReport
  -> UnresolvedReport
recordUnresolved fundId assetId =
  M.insertWith S.union fundId (S.singleton assetId)

writeReport :: FilePath -> UnresolvedReport -> IO ()
writeReport path report =
  TIO.writeFile path (render report)

render :: UnresolvedReport -> T.Text
render =
  T.unlines . concatMap renderFund . M.toList
  where
    renderFund (FundId fid, ids) =
      ("Fund: " <> fid)
        : map (\(RawAssetId a) -> "  - " <> a)
              (S.toList ids)
