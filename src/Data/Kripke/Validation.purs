module Data.Kripke.Validation (Errors, Validation, toEither, validateReflexive, validateTransitive, validateMonotonic) where
  
import Prelude

import Data.Array (filter)
import Data.Either (Either(..))
import Data.Foldable (and)
import Data.Kripke.Kripke (KripkeFrame, Model, testK)
import Data.Tuple (Tuple(..))
import Data.Validation.Semigroup (V, invalid, unV)

type Errors = Array String

type Validation = V Errors Unit

vMap :: String -> Boolean -> Validation
vMap err test
  | test = pure unit
  | otherwise = invalid [err]


toEither :: forall a b. V a b -> Either a b
toEither = unV Left Right

validateReflexive :: KripkeFrame -> Validation
validateReflexive { worlds, relation } = vMap "The accessibility relation is not reflexive" isReflexive
  where reflexInRelation = \n -> testK relation n n
        isReflexive = and $ map reflexInRelation worlds

validateTransitive :: KripkeFrame -> Validation
validateTransitive { worlds, relation } = vMap "The accessibility relation is not transitive" isTransitive
  where isTransitive = and $ do
          (Tuple x y) <- relation
          z <- filter (testK relation y) worlds
          pure (testK relation x z)

validateMonotonic :: Model -> Validation
validateMonotonic { frame: { worlds, relation }, valuation } = vMap "The valuation is not monotonic" isMonotonic
  where isMonotonic = and $ do
          (Tuple atom w) <- valuation
          w' <- filter (testK relation w) worlds
          pure (testK valuation atom w')