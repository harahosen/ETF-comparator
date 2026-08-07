module Service.Config
  ( Config(..)
  , loadConfig
  , loadConfigFromFile
  , defaultConfig
  ) where

import Data.Yaml (FromJSON, decodeFileEither)
import Data.Text (Text)
import GHC.Generics (Generic)
import System.Directory (getCurrentDirectory)

data Config = Config
  { assetMappingFile :: FilePath  -- Path to asset ID mapping CSV file
  , inputDirectory   :: FilePath  -- Mock: Input directory for ETF files (currently unused)
  , outputDirectory  :: FilePath  -- Mock: Output directory for processed ETF files (currently unused)
  , tolerance        :: Double    -- Tolerance for floating-point comparisons in normalization
  , failOnUnresolved :: Bool      -- Whether to fail if asset IDs cannot be resolved
  } deriving (Show, Generic)

instance FromJSON Config

defaultConfig :: Config
defaultConfig = Config
  { assetMappingFile = "Input/asset-mapping.csv"
  , inputDirectory  = "Input"
  , outputDirectory = "Output"
  , tolerance       = 1e-6
  , failOnUnresolved = False
  }

loadConfig :: IO (Either String Config)
loadConfig = loadConfigFromFile "config.yaml"

loadConfigFromFile :: FilePath -> IO (Either String Config)
loadConfigFromFile path = do
  result <- decodeFileEither path
  case result of
    Left err -> return $ Left (show err)
    Right cfg -> return $ Right cfg