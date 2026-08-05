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
import Domain.Validation (validateRawETF, validateRawETFAllErrors)
import Domain.Normalization (normalizeETFWithTolerance)
import Domain.Merge
import Domain.Comparison
import Ingestion.FileLoader
import Ingestion.MappingLoader
import Ingestion.ResolveAssetId
import Ingestion.UnsolvedId
import Control.Monad.Except
import Control.Monad.IO.Class

data ComparisonMetrics = ComparisonMetrics
  { cosineSimilarityValue :: Double
  , weightedJaccardSimilarityValue :: Double
  , overlapRatioValue :: Double
  } deriving (Show)

-- | 'ComparisonError' now carries a list of failed files, each with its own
-- first error message and the full list of collected errors.  This allows the
-- output to contain one row per failed file.
data ComparisonResult
  = ComparisonSuccess ComparisonMetrics [(String, [String])]
  | ComparisonError [(String, FilePath, [PipelineError])]
  deriving (Show)

-- Helper function to process a single ETF and collect all errors
processSingleETFWithErrors :: Config -> FilePath -> IO (Either (String, [PipelineError]) (NormalizedETF, [RawAssetId], FundId, [String]))
processSingleETFWithErrors config filePath = do
  -- Load asset mapping
  mappingResult <- loadAssetMapping (assetMappingFile config)
  case mappingResult of
    Left err -> return $ Left (err, [LoadPE err])
    Right mapping -> do
      -- Load and parse ETF
      loadResult <- loadETF filePath
      case loadResult of
        Left err -> return $ Left (err, [LoadPE err])
        Right rawEtf -> do
          let fundId = etfFundId rawEtf

          -- Resolve asset IDs
          let (resolvedEtf, unresolvedIds) = resolveETFAssetIds mapping rawEtf

          -- Report unresolved IDs
          reportUnsolved fundId unresolvedIds

          -- Validate before merging so that duplicate / per-row errors are detected
          case validateRawETFAllErrors resolvedEtf of
            Left (err:errs) -> return $ Left (show err, map ValidationPE (err:errs))
            Left [] -> return $ Left ("Unknown validation error", [])
            Right _ -> do
              -- Merge holdings
              let mergedEtf = mergeResolvedHoldings resolvedEtf

              -- Normalize with configured tolerance
              case normalizeETFWithTolerance (tolerance config) mergedEtf of
                Left normErr -> return $ Left (show normErr, [NormalizationPE normErr])
                Right normalizedEtf -> return $ Right (normalizedEtf, unresolvedIds, fundId, [])

-- Helper function to process a single ETF (standard version)
processSingleETF :: Config -> FilePath -> ExceptT String IO (NormalizedETF, [RawAssetId], FundId)
processSingleETF config filePath = do
  -- Load asset mapping
  mappingResult <- liftIO (loadAssetMapping (assetMappingFile config))
  mapping <- case mappingResult of
    Left err -> throwError err
    Right m -> return m

  -- Load and parse ETF
  rawEtf <- ExceptT (liftIO (loadETF filePath))

  -- Resolve asset IDs
  let (resolvedEtf, unresolvedIds) = resolveETFAssetIds mapping rawEtf
  let fundId = etfFundId rawEtf

  -- Report unresolved IDs
  liftIO $ reportUnsolved fundId unresolvedIds

  -- Validate before merging
  _ <- case validateRawETF resolvedEtf of
    Left errs -> throwError (show errs)
    Right etf -> return etf

  -- Merge holdings
  let mergedEtf = mergeResolvedHoldings resolvedEtf

  -- Normalize with configured tolerance
  normalizedEtf <- case normalizeETFWithTolerance (tolerance config) mergedEtf of
    Left normErr -> throwError (show normErr)
    Right etf -> return etf

  return (normalizedEtf, unresolvedIds, fundId)

processComparisonWithConfig :: Config -> FilePath -> FilePath -> IO ComparisonResult
processComparisonWithConfig config file1 file2 = do
  -- Process both ETFs separately to track which one fails and collect all errors
  result1 <- processSingleETFWithErrors config file1
  result2 <- processSingleETFWithErrors config file2

  case (result1, result2) of
    (Right (etf1, unresolved1, fundId1, _), Right (etf2, unresolved2, fundId2, _)) -> do
      -- Both succeeded, perform comparison
      let cosSim = cosineSimilarity etf1 etf2
          jacSim = weightedJaccardSimilarity etf1 etf2
          overlap = overlapRatio etf1 etf2
          unresolved = [(unFundId fundId1, map unRawAssetId unresolved1),
                        (unFundId fundId2, map unRawAssetId unresolved2)]

      return $ ComparisonSuccess (ComparisonMetrics
        { cosineSimilarityValue = cosSim
        , weightedJaccardSimilarityValue = jacSim
        , overlapRatioValue = overlap
        }) unresolved

    (Left (firstError, allErrors), Right _) ->
      return $ ComparisonError [(firstError, file1, allErrors)]

    (Right _, Left (firstError, allErrors)) ->
      return $ ComparisonError [(firstError, file2, allErrors)]

    (Left (firstError1, allErrors1), Left (firstError2, allErrors2)) ->
      return $ ComparisonError [(firstError1, file1, allErrors1), (firstError2, file2, allErrors2)]

processComparison :: FilePath -> FilePath -> IO ComparisonResult
processComparison = processComparisonWithConfig defaultConfig
