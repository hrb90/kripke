module Data.Kripke.Validation (Errors, Validation, validateReflexive) where
  
import Prelude

import Data.Foldable (and, elem)
import Data.Kripke.Kripke
import Data.Tuple (Tuple(..))
import Data.Validation.Semigroup (V, invalid)

type Errors = Array String

type Validation = V Errors Unit

vMap :: String -> Boolean -> Validation
vMap err test
  | test = pure unit
  | otherwise = invalid [err]

validateReflexive :: KripkeFrame -> Validation
validateReflexive { worlds, relation } = vMap "The accessibility relation is not reflexive" isReflexive
  where reflexInRelation node = elem (Tuple node node) relation
        isReflexive = and $ map reflexInRelation worlds