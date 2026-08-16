module Service.Config
  ( Config(..)
  , loadConfigFromFile
  , defaultConfig
  ) where

import Data.Yaml (FromJSON, decodeFileEither)
import GHC.Generics (Generic)

data Config = Config
  { assetMappingFile :: FilePath  -- Path to asset ID mapping CSV file
  , outputDirectory  :: FilePath  -- Output directory for result CSV files
  , tolerance        :: Double    -- Tolerance for floating-point comparisons in normalization
  } deriving (Show, Generic)

instance FromJSON Config

defaultConfig :: Config
defaultConfig = Config
  { assetMappingFile = "input/asset-mapping.csv"
  , outputDirectory = "output"
  , tolerance       = 1e-6
  }

loadConfigFromFile :: FilePath -> IO (Either String Config)
loadConfigFromFile path = do
  result <- decodeFileEither path
  case result of
    Left err -> return $ Left (show err)
    Right cfg -> return $ Right cfg