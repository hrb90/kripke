module Data.Kripke.Validation 
  ( Errors
  , Validation
  , toEither
  , validateReflexive
  , validateTransitive
  , validateMonotonic
  , validateDomain
  , validateTotal
  , validateEuclidean ) where
  
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

-- Checks that a Kripke frame is reflexive, i.e., every world is accessible from itself
validateReflexive :: KripkeFrame -> Validation
validateReflexive { worlds, relation } = vMap "The accessibility relation is not reflexive" isReflexive
  where reflexInRelation = \n -> testK relation n n
        isReflexive = and $ map reflexInRelation worlds

-- Checks that a Kripke frame is transitive, i.e., if v is accessible from u, 
-- and w is accessible from v, then w is accessible from u
validateTransitive :: KripkeFrame -> Validation
validateTransitive { worlds, relation } = vMap "The accessibility relation is not transitive" isTransitive
  where isTransitive = and $ do
          (Tuple u v) <- relation
          w <- filter (testK relation v) worlds
          pure $ testK relation u w

-- Checks that a Kripke frame is Euclidean, i.e., if u and v are both accessible from w,
-- then they are accessible from each other.
validateEuclidean :: KripkeFrame -> Validation
validateEuclidean { worlds, relation } = vMap "The accessibility relation is not Euclidean" isEuclidean
  where isEuclidean = and $ do
          w <- worlds
          u <- filter (testK relation w) worlds
          v <- filter (testK relation w) worlds
          pure $ testK relation u v

-- Checks that a Kripke frame is total, i.e., for every pair of worlds w, v,
-- one of the worlds is accessible from the other.
validateTotal :: KripkeFrame -> Validation
validateTotal { worlds, relation } = vMap "The accessibility relation is not total" isTotal
  where isTotal = and $ do
          w <- worlds
          v <- worlds
          pure $ (testK relation w v || testK relation v w)


-- Checks that the domain is monotonic, i.e., if a variable x is in the domain of w, 
-- and u is accessible from w, x is in the domain of u.
validateMonotonicD :: Model -> Validation
validateMonotonicD { frame: { worlds, relation }, domain } = vMap "The domain is not monotonic" isMonotonic
  where isMonotonic = and $ do
          (Tuple atom w) <- domain
          w' <- filter (testK relation w) worlds
          pure $ testK domain atom w'

-- Same as above, but for the valuation.
validateMonotonicV :: Model -> Validation
validateMonotonicV { frame: { worlds, relation }, valuation } = vMap "The valuation is not monotonic" isMonotonic
  where isMonotonic = and $ do
          (Tuple atom w) <- valuation
          w' <- filter (testK relation w) worlds
          pure $ testK valuation atom w'

validateMonotonic :: Model -> Validation
validateMonotonic m = validateMonotonicD m *> validateMonotonicV m

-- Checks that the valuation for each world is a subset of the domain
validateDomain :: Model -> Validation
validateDomain { valuation, domain } = vMap "There are valuations for variables not in the domain of the corresponding world" domainMakesSense
  where domainMakesSense = and $ map (\(Tuple a b) -> testK domain a b) valuation