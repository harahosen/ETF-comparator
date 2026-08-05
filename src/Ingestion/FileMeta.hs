module Ingestion.FileMeta
  ( ProviderCode(..)
  , FileFormat(..)
  , FileMeta(..)
  , deriveFileMetadata
  ) where

import System.FilePath (takeExtension, takeBaseName)
import Data.Char (isDigit, toLower)

-- data provider
data ProviderCode
  = IS   -- iShares
  | SS   -- State Street
  | CF   -- Custom File
  deriving (Eq, Show)

-- file format
data FileFormat
  = CSV
  deriving (Eq, Show)

-- file metadata
data FileMeta = FileMeta
  { fmDate     :: String        -- YYYYMMDD, syntactically validated
  , fmProvider :: ProviderCode
  , fmFormat   :: FileFormat
  , fmPath     :: FilePath
  } deriving (Eq, Show)

-- metadata derivation from the filename
deriveFileMetadata :: FilePath -> Either String FileMeta
deriveFileMetadata path = do
  format   <- checkFormat path
  (d, p)   <- checkFilename path
  pure FileMeta
    { fmDate     = d
    , fmProvider = p
    , fmFormat   = format
    , fmPath     = path
    }

checkFormat :: FilePath -> Either String FileFormat
checkFormat path =
  case map toLower (takeExtension path) of
    ".csv"  -> Right CSV
    ext -> Left ("Unsupported file extension: " <> ext)

checkFilename :: FilePath -> Either String (String, ProviderCode)
checkFilename path =
  case fileSplit (takeBaseName path) of
    (dateStr : providerStr : _) -> do
      checkDate dateStr
      provider <- parseProvider providerStr
      Right (dateStr, provider)
    _ ->
      Left "Filename must be YYYYMMDD-PROVIDER-<name>"

fileSplit :: String -> [String]
fileSplit [] = []
fileSplit xs =
  let (h, rest) = span (/= '-') xs
  in h : case rest of
           [] -> []
           (_:rs) -> fileSplit rs

checkDate :: String -> Either String ()
checkDate s
  | length s == 8 && all isDigit s = Right ()
  | otherwise = Left ("Invalid date in filename: " <> s)

parseProvider :: String -> Either String ProviderCode
parseProvider provider = case provider of 
  "IS" -> Right IS;
  "SS" -> Right SS;
  "CF" -> Right CF;
  p    -> Left ("Unknown provider code: " <> p)