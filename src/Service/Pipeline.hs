module Service.Pipeline
  ( ComparisonResult(..)
  , ComparisonMetrics(ComparisonMetrics, cosineSimilarityValue, weightedJaccardSimilarityValue, overlapRatioValue)
  , PipelineError(..)
  , processComparison
  , processComparisonWithConfig
  ) where

import Service.Config
import Domain.Types
import Domain.Errors (PipelineError(..))
import Domain.Validation (validateRawETF)
import Domain.Normalization (normalizeETFWithTolerance)
import Domain.Merge (mergeResolvedHoldings)
import Domain.Comparison (cosineSimilarity, weightedJaccardSimilarity, overlapRatio)
import Ingestion.AssetIdMapping (AssetIdMapping)
import Ingestion.FileMeta (deriveFileMetadata, fmDate)
import Ingestion.TableLoader (loadTable)
import Ingestion.Reconcile (reconcileTables)
import Ingestion.MappingLoader (loadAssetMapping)
import Ingestion.ResolveAssetId (resolveETFAssetIds)
import Parser.Reconciled (parseReconciled)

import Control.Exception (try, IOException)
import Data.Text (Text)

type Table = [[Text]]

data ComparisonMetrics = ComparisonMetrics
  { cosineSimilarityValue :: Double
  , weightedJaccardSimilarityValue :: Double
  , overlapRatioValue :: Double
  } deriving (Show)

data ComparisonResult
  = ComparisonSuccess ComparisonMetrics
  | ComparisonError [(String, FilePath, [PipelineError])]
  deriving (Show)

processComparisonWithConfig :: Config -> FilePath -> FilePath -> IO ComparisonResult
processComparisonWithConfig config file1 file2 = do
  case (deriveFileMetadata file1, deriveFileMetadata file2) of
    (Left err, _) ->
      return $ ComparisonError [(err, file1, [LoadPE err])]
    (_, Left err) ->
      return $ ComparisonError [(err, file2, [LoadPE err])]
    (Right meta1, Right meta2) -> do
      eTable1 <- try (loadTable meta1) :: IO (Either IOException Table)
      eTable2 <- try (loadTable meta2) :: IO (Either IOException Table)
      case (eTable1, eTable2) of
        (Left ioe, _) ->
          return $ ComparisonError [(show ioe, file1, [LoadPE (show ioe)])]
        (_, Left ioe) ->
          return $ ComparisonError [(show ioe, file2, [LoadPE (show ioe)])]
        (Right table1, Right table2) ->
          case reconcileTables file1 file2 table1 table2 of
            Left err ->
              return $ ComparisonError [(err, file1 ++ " and " ++ file2, [LoadPE err])]
            Right (primaryTable, secondaryTable) ->
              case (parseReconciled (FundId (fmDate meta1)) primaryTable,
                    parseReconciled (FundId (fmDate meta2)) secondaryTable) of
                (Left err, _) ->
                  return $ ComparisonError [(err, file1, [LoadPE err])]
                (_, Left err) ->
                  return $ ComparisonError [(err, file2, [LoadPE err])]
                (Right raw1, Right raw2) -> do
                  mappingResult <- loadAssetMapping (assetMappingFile config)
                  case mappingResult of
                    Left err ->
                      return $ ComparisonError [(err, assetMappingFile config, [LoadPE err])]
                    Right mapping ->
                      case (processSingleETF config mapping raw1, processSingleETF config mapping raw2) of
                        (Left (firstErr, allErrs), Right _) ->
                          return $ ComparisonError [(firstErr, file1, allErrs)]
                        (Right _, Left (firstErr, allErrs)) ->
                          return $ ComparisonError [(firstErr, file2, allErrs)]
                        (Left (firstErr1, allErrs1), Left (firstErr2, allErrs2)) ->
                          return $ ComparisonError
                            [ (firstErr1, file1, allErrs1)
                            , (firstErr2, file2, allErrs2)
                            ]
                        (Right (etf1, _), Right (etf2, _)) -> do
                          let cosSim = cosineSimilarity etf1 etf2
                              jacSim = weightedJaccardSimilarity etf1 etf2
                              overlap = overlapRatio etf1 etf2
                          return $ ComparisonSuccess (ComparisonMetrics
                            { cosineSimilarityValue = cosSim
                            , weightedJaccardSimilarityValue = jacSim
                            , overlapRatioValue = overlap
                            })

processSingleETF :: Config -> AssetIdMapping -> RawETF -> Either (String, [PipelineError]) (NormalizedETF, FundId)
processSingleETF config mapping rawEtf =
  let resolvedEtf = resolveETFAssetIds mapping rawEtf
  in case validateRawETF resolvedEtf of
       Left (err:errs) ->
         Left (show err, map ValidationPE (err:errs))
       Left [] ->
         Left ("Unknown validation error", [])
       Right _ ->
         let mergedEtf = mergeResolvedHoldings resolvedEtf
         in case normalizeETFWithTolerance (tolerance config) mergedEtf of
              Left normErr ->
                Left (show normErr, [NormalizationPE normErr])
              Right normalizedEtf ->
                Right (normalizedEtf, etfFundId rawEtf)

processComparison :: FilePath -> FilePath -> IO ComparisonResult
processComparison = processComparisonWithConfig defaultConfig
