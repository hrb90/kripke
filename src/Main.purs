module Main where

import Prelude

import Control.Monad.Eff (Eff)
import DOM (DOM)
import Data.Kripke.Kripke (Model)
import Logics.Intuitionistic.Validation (validate)
import Signal.Channel (CHANNEL)
import Sparkle (sparkle)

readModel :: Model -> Unit
readModel _ = unit

main :: forall eff. Eff ( channel :: CHANNEL, dom :: DOM | eff ) Unit
main = sparkle "model validation" validate