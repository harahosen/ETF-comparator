module Parser.Common.ParserHelpers
  ( normalizeHeader
  , findColumnByPriority
  , parseDouble
  ) where

import Data.Char (isAlphaNum)
import Data.List (findIndex)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Read (readMaybe)

normalizeHeader :: Text -> Text
normalizeHeader =
  T.toLower . T.filter (\c -> isAlphaNum c || c == ' ')

findColumnByPriority :: [Text] -> [Text] -> Either String Int
findColumnByPriority priorities header =
  case [ ix
       | key <- priorities
       , ix  <- maybeToList (findIndex (matches key) header)
       ] of
    (i:_) -> Right i
    []    -> Left ("Missing required column. Tried: " <> show priorities)
  where
    matches k h = normalizeHeader h == normalizeHeader k
    maybeToList = maybe [] pure

parseDouble :: Text -> Either String Double
parseDouble t =
  maybe (Left ("Invalid numeric value: " <> T.unpack t)) Right
        (readMaybe (T.unpack t))
