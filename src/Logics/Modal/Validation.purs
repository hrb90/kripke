module Logics.Modal.Validation (validateK, validateT, validateS4, validateS5) where

import Prelude

import Data.Either (Either)
import Data.Kripke.Kripke (Model)
import Data.Kripke.Validation (Validation, toEither, validateDomain, validateEuclidean, validateMonotonic, validateReflexive, validateTransitive)


-- Every classical modal logic with a Kripke semantics 
-- necessarily includes the axiom
--    [] (p -> q) -> ([] p -> [] q)
-- and the inference fule
--      |- p
--   ----------
--    |- [] p
validateK' :: Model -> Validation
validateK' m = validateDomain m  *> validateMonotonic m

-- The modal logic K
-- The most minimal modal logic
validateK :: Model -> Either (Array String) Unit
validateK = toEither <<< validateK'

-- Alternative names based on the names of the axioms of modal logic
validateT' = validateReflexive
validate4 = validateTransitive
validate5 = validateEuclidean

-- The modal logic T: K plus axiom T
--    [] p -> p
validateT :: Model -> Either (Array String) Unit
validateT m@{ frame } = toEither $ validateK' m
                                    *> validateT' frame

-- The modal logic S4: T plus axiom 4
--    [] p -> [] [] p 
validateS4 :: Model -> Either (Array String) Unit
validateS4 m@{ frame } = toEither $ validateK' m
                                      *> validateT' frame
                                      *> validate4 frame

-- The modal logic S5: S4 plus axiom 5
--    <> p -> [] <> p
validateS5 :: Model -> Either (Array String) Unit
validateS5 m@{ frame } = toEither $ validateK' m
                                      *> validateT' frame
                                      *> validate4 frame
                                      *> validate5 frame