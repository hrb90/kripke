module Data.Kripke.Validation (Errors, Validation, validateReflexive, validateTransitive, validateMonotonic) where
  
import Prelude

import Data.Array (filter)
import Data.Foldable (and, elem)
import Data.Kripke.Kripke (KripkeFrame, Model, Node, Relation)
import Data.Tuple (Tuple(..))
import Data.Validation.Semigroup (V, invalid)

type Errors = Array String

type Validation = V Errors Unit

vMap :: String -> Boolean -> Validation
vMap err test
  | test = pure unit
  | otherwise = invalid [err]

testAccessibility :: Relation -> Node -> Node -> Boolean
testAccessibility r w w' = elem (Tuple w w') r

validateReflexive :: KripkeFrame -> Validation
validateReflexive { worlds, relation } = vMap "The accessibility relation is not reflexive" isReflexive
  where reflexInRelation = \n -> testAccessibility relation n n
        isReflexive = and $ map reflexInRelation worlds

validateTransitive :: KripkeFrame -> Validation
validateTransitive { worlds, relation } = vMap "The accessibility relation is not transitive" isTransitive
  where accessible = testAccessibility relation
        isTransitive = and $ do
          (Tuple x y) <- relation
          z <- filter (accessible y) worlds
          pure (accessible x z)

validateMonotonic :: Model -> Validation
validateMonotonic { frame: { worlds, relation }, valuation } = vMap "The valuation is not monotonic" isMonotonic
  where accessible = testAccessibility relation
        testAtom a w = elem (Tuple a w) valuation
        isMonotonic = and $ do
          (Tuple atom w) <- valuation
          w' <- filter (accessible w) worlds
          pure (testAtom atom w')