module Parser.Common.PreprocessHelpers
  ( dropEmptyRows
  , trimTable
  ) where

import Data.Text (Text)
import qualified Data.Text as T

-- | Drop rows that are entirely empty or whitespace
dropEmptyRows :: [[Text]] -> [[Text]]
dropEmptyRows =
  filter (not . all (T.null . T.strip))

-- | Trim leading and trailing empty rows
trimTable :: [[Text]] -> [[Text]]
trimTable =
  dropWhileEnd isEmpty . dropWhile isEmpty
  where
    isEmpty = all (T.null . T.strip)
    dropWhileEnd p = reverse . dropWhile p . reverse
