module Service.Pipeline
  ( ProcessResult(..)
  , processETF
  , processETFWithConfig
  ) where

import Service.Config
import Domain.Types
import Domain.Validation
import Domain.Normalization (normalizeETFWithTolerance)
import Domain.Merge
import Ingestion.FileLoader
import Ingestion.MappingLoader
import Ingestion.FundMappingLoader
import Ingestion.ResolveAssetId
import Ingestion.ResolveFundId
import Ingestion.UnsolvedId
import Control.Monad.Except
import Control.Monad.IO.Class

data ProcessResult
  = ProcessSuccess NormalizedETF
  | ProcessValidationError [ValidationError]    
  | ProcessNormalizationError NormalizationError
  | ProcessMappingError String
  | ProcessUnresolvedAssetIds [RawAssetId]
  | ProcessUnresolvedFundId RawFundId
  deriving (Show)

processETFWithConfig :: Config -> FilePath -> IO ProcessResult
processETFWithConfig config filePath = runExceptT $ do
  -- Load asset mapping
  mappingResult <- liftIO (loadAssetMapping (assetMappingFile config))
  mapping <- case mappingResult of
    Left err -> throwError (ProcessMappingError err)
    Right m -> return m
  
  -- Load fund mapping
  fundMappingResult <- liftIO (loadFundMapping (fundMappingFile config))
  fundMapping <- case fundMappingResult of
    Left err -> throwError (ProcessMappingError err)
    Right fm -> return fm
  
  -- Load and parse ETF
  rawEtf <- ExceptT (liftIO (loadETF filePath))
  
  -- Resolve fund ID
  resolvedFundEtf <- case resolveETFFundId fundMapping rawEtf of
    Left unresolvedFundId -> throwError (ProcessUnresolvedFundId unresolvedFundId)
    Right etf -> return etf
  
  -- Resolve asset IDs
  let (resolvedEtf, unresolvedIds) = resolveETFAssetIds mapping resolvedFundEtf
  
  -- Handle unresolved IDs based on config
  when (failOnUnresolved config && not (null unresolvedIds)) $
    throwError (ProcessUnresolvedAssetIds unresolvedIds)
  
  -- Report unresolved IDs (even if not failing)
  liftIO $ reportUnsolved (etfRawFundId resolvedEtf) unresolvedIds
  
  -- Merge holdings
  let mergedEtf = mergeResolvedHoldings resolvedEtf
  
  -- Validate
  case validateRawETF mergedEtf of
    Left errs -> throwError (ProcessValidationError errs)
    Right validatedEtf -> do
      -- Normalize with configured tolerance
      case normalizeETFWithTolerance (tolerance config) validatedEtf of
        Left normErr -> throwError (ProcessNormalizationError normErr)
        Right normalizedEtf -> return (ProcessSuccess normalizedEtf)

processETF :: FilePath -> IO ProcessResult
processETF = processETFWithConfig defaultConfig