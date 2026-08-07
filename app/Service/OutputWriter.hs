module Service.OutputWriter
  ( OutputResult(..)
  , ComparisonOutput(..)
  , ErrorOutput(..)
  , writeOutput
  , writeComparisonOutput
  , writeErrorOutput
  , writeErrorOutputs
  , mkComparisonOutput
  , mkErrorOutput
  ) where

import Domain.Types
import Domain.Errors (PipelineError(..))
import Service.Pipeline (ComparisonMetrics(..))
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS8
import qualified Data.ByteString.Char8 as BC
import Data.Aeson (encode)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import System.FilePath ((</>), makeRelative)
import System.Directory (createDirectoryIfMissing)
import Data.List (intercalate)

-- Output result types
data OutputResult
  = OutputSuccess ComparisonOutput
  | OutputFailure [ErrorOutput]
  deriving (Show, Eq)

-- Successful comparison output
data ComparisonOutput = ComparisonOutput
  { coTimestamp :: String
  , coFile1 :: FilePath
  , coFile2 :: FilePath
  , coCosineSimilarity :: Double
  , coWeightedJaccardSimilarity :: Double
  , coOverlapRatio :: Double
  , coUnresolvedIds :: [(String, [String])]  -- (FundId, [RawAssetId])
  } deriving (Show, Eq)

-- Error output for a single failed file
data ErrorOutput = ErrorOutput
  { eoTimestamp :: String
  , eoFailedFile :: FilePath
  , eoErrorList :: [PipelineError]
  } deriving (Show, Eq)

-- Write output based on result
writeOutput :: FilePath -> OutputResult -> IO ()
writeOutput outputDir result = do
  createDirectoryIfMissing True outputDir
  timestamp <- getCurrentTime
  let timestampStr = formatTime defaultTimeLocale "%Y%m%d-%H%M%S" timestamp
  case result of
    OutputSuccess compOut -> writeComparisonOutput outputDir timestampStr compOut
    OutputFailure errOuts -> writeErrorOutputs outputDir timestampStr errOuts

-- Write successful comparison output
writeComparisonOutput :: FilePath -> String -> ComparisonOutput -> IO ()
writeComparisonOutput outputDir timestamp compOut = do
  let filePath = outputDir </> "comparison-" ++ timestamp ++ ".csv"
  let header = "timestamp,etf_file_1,etf_file_2,cosine_similarity,weighted_jaccard_similarity,overlap_ratio"
  let relFile1 = makeRelative "." (coFile1 compOut)
  let relFile2 = makeRelative "." (coFile2 compOut)
  let row = intercalate "," [coTimestamp compOut, relFile1, relFile2,
                    show (coCosineSimilarity compOut), show (coWeightedJaccardSimilarity compOut),
                    show (coOverlapRatio compOut)]
  let content = BC.pack $ unlines [header, row]
  LBS.writeFile filePath (LBS.fromStrict content)

-- Write a single error row (convenience wrapper around 'writeErrorOutputs')
writeErrorOutput :: FilePath -> String -> ErrorOutput -> IO ()
writeErrorOutput outputDir timestamp errOut =
  writeErrorOutputs outputDir timestamp [errOut]

-- Write error output CSV, one row per failed file
writeErrorOutputs :: FilePath -> String -> [ErrorOutput] -> IO ()
writeErrorOutputs outputDir timestamp errOuts = do
  let filePath = outputDir </> "error-" ++ timestamp ++ ".csv"
  let header = "timestamp,error_file,error_list"
  let rows = map errorRow errOuts
  let content = BC.pack $ unlines (header : rows)
  LBS.writeFile filePath (LBS.fromStrict content)
  where
    errorRow errOut =
      let relFailedFile = makeRelative "." (eoFailedFile errOut)
          errorListJson = LBS8.unpack (encode (eoErrorList errOut))
          errorListField = "\"" ++ concatMap (\c -> if c == '"' then "\"\"" else [c]) errorListJson ++ "\""
      in intercalate "," [eoTimestamp errOut, relFailedFile, errorListField]

-- Helper to create ComparisonOutput from pipeline result
mkComparisonOutput :: String -> String -> ComparisonMetrics -> [(String, [String])] -> IO ComparisonOutput
mkComparisonOutput file1 file2 metrics unresolved = do
  timestamp <- getCurrentTime
  let timestampStr = formatTime defaultTimeLocale "%Y%m%d-%H%M%S" timestamp
  return $ ComparisonOutput
    { coTimestamp = timestampStr
    , coFile1 = file1
    , coFile2 = file2
    , coCosineSimilarity = cosineSimilarityValue metrics
    , coWeightedJaccardSimilarity = weightedJaccardSimilarityValue metrics
    , coOverlapRatio = overlapRatioValue metrics
    , coUnresolvedIds = unresolved
    }

-- Helper to create ErrorOutput for a failed file
mkErrorOutput :: FilePath -> [PipelineError] -> IO ErrorOutput
mkErrorOutput failedFile errorList = do
  timestamp <- getCurrentTime
  let timestampStr = formatTime defaultTimeLocale "%Y%m%d-%H%M%S" timestamp
  return $ ErrorOutput
    { eoTimestamp = timestampStr
    , eoFailedFile = failedFile
    , eoErrorList = errorList
    }
