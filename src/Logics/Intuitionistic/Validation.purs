module Logics.Intuitionistic.Validation (validate) where
  
import Prelude

import Data.Either (Either)
import Data.Kripke.Kripke (Model)
import Data.Kripke.Validation  ( validateMonotonic
                               , validateReflexive
                               , validateTransitive
                               , validateDomain
                               , toEither )

validate :: Model -> Either (Array String) Unit
validate m@{ frame } = toEither $ (validateDomain m) *> (validateReflexive frame) *> (validateTransitive frame) *> (validateMonotonic m)