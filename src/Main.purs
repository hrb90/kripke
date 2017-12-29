module Main where

import Prelude

import Control.Monad.Eff (Eff)
import DOM (DOM)
import Data.Array (singleton)
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Kripke.Kripke (Model, Node, Evaluation, runEvaluation)
import Data.Kripke.Transformations (reflexiveClosure, transitiveClosure)
import Logics.Intuitionistic.Propositional (exprParser, evaluation)
import Signal.Channel (CHANNEL)
import Sparkle (sparkle)
import Glitter (glit)

combine :: forall err expr. (String -> Either err expr) -> (Evaluation err expr) -> Model -> Node -> String -> Either err Boolean
combine parser eval m n s = parser s >>= evaluate (close m) n where 
      evaluate = runEvaluation eval
      close { frame, valuation, domain } =
            { frame: reflexiveClosure $ transitiveClosure $ frame 
            , valuation
            , domain }

main :: forall eff. Eff ( channel :: CHANNEL, dom :: DOM | eff ) Unit
main = sparkle "model validation" $ combine parse evaluation where
      parse = glit exprParser # map (lmap singleton)