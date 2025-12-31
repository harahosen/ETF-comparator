module Ingestion.FileMeta
  ( Vendor(..)
  , FileFormat(..)
  , FileMeta(..)
  , inferFileMeta
  ) where

import System.FilePath (takeExtension, takeFileName)
import Data.Char (toLower)

-- data provider
data Provider
  = IShares
  | StateStreet
  deriving (Eq, Show)

-- file format
data FileFormat
  = CSV
  | XLSX
  deriving (Eq, Show)

-- metadata
data FileMeta = FileMeta
  { fmVendor :: Provider
  , fmFormat :: FileFormat
  , fmPath   :: FilePath
  } deriving (Eq, Show)

-- provider and format inference from file name and extension
inferFileMeta :: FilePath -> Either String FileMeta
inferFileMeta path =
  let ext  = map toLower (takeExtension path)
      name = map toLower (takeFileName path)
  in case (ext, name) of
       (".csv", n) | "ivv" `elem` wordsByNonAlpha n ->
         Right (FileMeta IShares CSV path)

       (".xlsx", n) | "spy" `elem` wordsByNonAlpha n ->
         Right (FileMeta StateStreet XLSX path)

       _ ->
         Left ("Unsupported ETF file: " <> path)

-- splitter of the file name
wordsByNonAlpha :: String -> [String]
wordsByNonAlpha =
  filter (not . null) . split
  where
    split [] = []
    split xs =
      let (w, rest) = span isAlpha xs
      in w : case rest of
               [] -> []
               (_:rs) -> split rs
    isAlpha = isAsciiLower
