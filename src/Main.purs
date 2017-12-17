module Main where

import Prelude

import Control.Monad.Eff (Eff)
import DOM (DOM)
import Data.Array (singleton)
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Kripke.Kripke (Model, Node, runEvaluation)
import Logics.Intuitionistic.Propositional (exprParser, evaluation)
import Signal.Channel (CHANNEL)
import Sparkle (sparkle)
import TestHelpers (glitter')

readModel :: Model -> Unit
readModel _ = unit

thingToSparkle :: Model -> Node -> String -> Either (Array String) Boolean
thingToSparkle m n s = do
      expr <- parse s
      evaluate m n expr
      where parse = glitter' exprParser # map (lmap singleton)
            evaluate = runEvaluation evaluation

main :: forall eff. Eff ( channel :: CHANNEL, dom :: DOM | eff ) Unit
main = sparkle "model validation" thingToSparkle