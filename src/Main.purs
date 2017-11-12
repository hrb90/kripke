module Main where

import Prelude
import Control.Monad.Eff (Eff)

import Data.Kripke.Kripke (Model, runEvaluation)
import Logics.Intuitionistic.Validation (validate)

import Sparkle (sparkle)

readModel :: Model -> Unit
readModel _ = unit

main = sparkle "model validation" validate