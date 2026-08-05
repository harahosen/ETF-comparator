module Main where

import Test.Hspec

import qualified Parser.CustomSpec
import qualified Parser.ISharesSpec
import qualified Parser.StateStreetSpec
import qualified Domain.ComparisonSpec
import qualified Domain.ValidationSpec
import qualified Domain.NormalizationSpec
import qualified Domain.MergeSpec
import qualified Ingestion.ResolveAssetIdSpec
import qualified Ingestion.FileMetaSpec
import qualified Ingestion.MappingLoaderSpec
import qualified Ingestion.FileLoaderSpec
import qualified Service.OutputWriterSpec
import qualified Service.PipelineSpec

main :: IO ()
main = hspec $ do
  Parser.CustomSpec.spec
  Parser.ISharesSpec.spec
  Parser.StateStreetSpec.spec
  Domain.ComparisonSpec.spec
  Domain.ValidationSpec.spec
  Domain.NormalizationSpec.spec
  Domain.MergeSpec.spec
  Ingestion.ResolveAssetIdSpec.spec
  Ingestion.FileMetaSpec.spec
  Ingestion.MappingLoaderSpec.spec
  Ingestion.FileLoaderSpec.spec
  Service.OutputWriterSpec.spec
  Service.PipelineSpec.spec
