module Logics.Intuitionistic.Validation (validate) where
  
import Prelude

import Data.Either (Either)
import Data.Kripke.Kripke (Model)
import Data.Kripke.Validation (validateMonotonic, validateReflexive, validateTransitive, toEither)

validate :: Model -> Either (Array String) Unit
validate m@{ frame } = toEither $ (validateReflexive frame) *> (validateTransitive frame) *> (validateMonotonic m)