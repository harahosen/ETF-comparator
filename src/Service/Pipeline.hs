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
import Domain.Reconcile (reconcileETFs)
import Ingestion.AssetIdMapping (AssetIdMapping)
import Ingestion.FileLoader (loadETF)
import Ingestion.MappingLoader (loadAssetMapping)
import Ingestion.ResolveAssetId (resolveETFAssetIds)

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
  result1 <- loadETF file1
  result2 <- loadETF file2
  case (result1, result2) of
    (Left err, _) ->
      return $ ComparisonError [(err, file1, [LoadPE err])]
    (_, Left err) ->
      return $ ComparisonError [(err, file2, [LoadPE err])]
    (Right raw1, Right raw2) ->
      case reconcileETFs raw1 raw2 of
        Left err ->
          return $ ComparisonError [(err, file1 ++ " and " ++ file2, [LoadPE err])]
        Right (reconciled1, reconciled2) -> do
          mappingResult <- loadAssetMapping (assetMappingFile config)
          case mappingResult of
            Left err ->
              return $ ComparisonError [(err, assetMappingFile config, [LoadPE err])]
            Right mapping ->
              case (processSingleETF config mapping reconciled1, processSingleETF config mapping reconciled2) of
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
