module Logics.Intuitionistic.Validation (validate, godelDummett) where

import Prelude

import Data.Either (Either)
import Data.Kripke.Kripke (Model)
import Data.Kripke.Validation (Validation, toEither, validateDomain, validateMonotonic, validateReflexive, validateTransitive, validateTotal)

validate' :: Model -> Validation
validate' m@{ frame } = validateDomain m
                          *> validateMonotonic m
                          *> validateReflexive frame
                          *> validateTransitive frame

validate :: Model -> Either (Array String) Unit
validate = toEither <<< validate' 

godelDummett' :: Model -> Validation
godelDummett' m@{ frame } = validate' m *> validateTotal frame

godelDummett :: Model -> Either (Array String) Unit
godelDummett = toEither <<< godelDummett'