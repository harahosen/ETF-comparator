module Domain.Validation
  ( validateRawETF
  ) where

import Domain.Types
import Domain.Errors

import qualified Data.Map.Strict as Map
import Data.List (foldl')
import Prelude hiding (isNaN)
import GHC.Float (isNaN)

-- same RawETF if OK, collection of validation errors if KO
validateRawETF :: RawETF -> Either [ValidationError] RawETF
validateRawETF etf@(RawETF _ hs) =
  etf <$ runRules hs validationRules

-- validation rule: returns zero or more errors
type ValidationRule = [Holding] -> Either [ValidationError] ()

-- list of rules to execute (in order)
validationRules :: [ValidationRule]
validationRules =
  [ ruleNonEmpty
  , checkDuplicatesAll
  , checkWeightsAll
  ]

-- all errors from the first rule that fails
runRules :: [Holding] -> [ValidationRule] -> Either [ValidationError] ()
runRules hs = foldl' (\acc rule -> acc >> rule hs) (Right ())

-- the holdings list must not be empty
ruleNonEmpty :: ValidationRule
ruleNonEmpty []  = Left [EmptyHoldings]
ruleNonEmpty _   = Right ()

-- returns a list of DuplicateHolding errors, one per duplicated CanonicalAssetId
checkDuplicatesAll :: ValidationRule
checkDuplicatesAll hs =
  let counts :: Map CanonicalAssetId Int
      counts =
        foldl'
          (\m h ->
            case holdingCanonicalId h of
              Just cid -> Map.insertWith (+) cid 1 m
              Nothing  -> m
          )
          Map.empty
          hs

      duplicates :: [ValidationError]
      duplicates =
        [ DuplicateHolding aid
        | (aid, c) <- Map.toList counts
        , c > 1
        ]
  in if null duplicates
       then Right ()
       else Left duplicates

-- the weights cannot be negative or NaN
checkWeightsAll :: ValidationRule
checkWeightsAll hs =
  let errs = concatMap checkWeightList hs
  in if null errs
       then Right ()
       else Left errs

-- validation for a single holding weight: returns a list with zero or one ValidationError.
checkWeightList :: Holding -> [ValidationError]
checkWeightList h =
  case (holdingCanonicalId h, holdingWeight h) of
    (Just cid, Weight w)
      | isNaN w -> [NonFiniteWeight cid]
      | w < 0   -> [NegativeWeight cid]
      | otherwise -> []
    _ -> []
