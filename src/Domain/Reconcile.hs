module Domain.Reconcile
  ( reconcileETFs
  ) where

import Domain.Types
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Set (Set)
import qualified Data.Text as T
import Data.Text (Text)
import Data.Char (isAlphaNum, toUpper)

reconcileETFs :: RawETF -> RawETF -> Either String (RawETF, RawETF)
reconcileETFs primary secondary =
  let slugMap = buildSlugMap primary
      reconciledHoldings = map (reconcileHolding slugMap) (etfHoldings secondary)
      mergedHoldings =
        M.elems $ M.fromListWith mergeHoldings
          [ (holdingRawId h, h) | h <- reconciledHoldings ]
  in Right (primary, RawETF (etfFundId secondary) mergedHoldings)

mergeHoldings :: Holding -> Holding -> Holding
mergeHoldings h1 h2 =
  h1 { holdingWeight = holdingWeight h1 <> holdingWeight h2 }

buildSlugMap :: RawETF -> M.Map Text String
buildSlugMap (RawETF _ hs) =
  M.fromListWith (\_ old -> old)
    [ (normalizeName n, unRawAssetId (holdingRawId h))
    | h <- hs
    , Just n <- [holdingName h]
    , not (T.null (T.strip n))
    ]

reconcileHolding :: M.Map Text String -> Holding -> Holding
reconcileHolding slugMap h =
  case holdingName h of
    Nothing -> h
    Just n  ->
      case M.lookup (normalizeName n) slugMap of
        Nothing -> h
        Just primaryAsset ->
          h { holdingRawId = RawAssetId primaryAsset
            , holdingName  = Nothing
            }

normalizeName :: Text -> Text
normalizeName =
  T.unwords
  . filter (`S.notMember` stopWords)
  . T.words
  . T.map (\c -> if isAlphaNum c then toUpper c else ' ')
  . T.strip

stopWords :: Set Text
stopWords = S.fromList
  [ "INC", "INCORPORATED", "CORP", "CORPORATION", "CO", "COMPANY", "PLC"
  , "LTD", "LIMITED", "LLC", "LP", "REIT", "TRUST", "CLASS", "A", "B", "C"
  , "W", "I", "WI", "WHEN", "ISSUED", "W-I", "W/I", "NEW", "SHARES"
  , "GROUP", "HOLDINGS", "HOLDING"
  ]
