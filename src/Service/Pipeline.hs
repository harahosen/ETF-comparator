module Service.Pipeline
  ( ComparisonResult(..)
  , ComparisonMetrics(..)
  , processComparison
  , processComparisonWithConfig
  ) where

import Service.Config
import Domain.Types
import Domain.Validation
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

data ComparisonResult
  = ComparisonSuccess ComparisonMetrics
  | ComparisonError String
  deriving (Show)

-- Helper function to process a single ETF
processSingleETF :: Config -> FilePath -> ExceptT String IO NormalizedETF
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
  
  -- Report unresolved IDs
  liftIO $ reportUnsolved (etfFundId resolvedEtf) unresolvedIds
  
  -- Merge holdings
  let mergedEtf = mergeResolvedHoldings resolvedEtf
  
  -- Validate
  validatedEtf <- case validateRawETF mergedEtf of
    Left errs -> throwError (show errs)
    Right etf -> return etf
  
  -- Normalize with configured tolerance
  normalizedEtf <- case normalizeETFWithTolerance (tolerance config) validatedEtf of
    Left normErr -> throwError (show normErr)
    Right etf -> return etf
  
  return normalizedEtf

processComparisonWithConfig :: Config -> FilePath -> FilePath -> IO ComparisonResult
processComparisonWithConfig config file1 file2 = do
  result <- runExceptT $ do
    -- Process both ETFs
    etf1 <- processSingleETF config file1
    etf2 <- processSingleETF config file2
    
    -- Perform comparison
    let cosSim = cosineSimilarity etf1 etf2
        jacSim = weightedJaccardSimilarity etf1 etf2
        overlap = overlapRatio etf1 etf2
    
    return $ ComparisonSuccess $ ComparisonMetrics
      { cosineSimilarityValue = cosSim
      , weightedJaccardSimilarityValue = jacSim
      , overlapRatioValue = overlap
      }
  case result of
    Left err -> return $ ComparisonError err
    Right success -> return success

processComparison :: FilePath -> FilePath -> IO ComparisonResult
processComparison = processComparisonWithConfig defaultConfig