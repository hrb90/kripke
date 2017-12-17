module Main where

import Prelude

import Control.Monad.Eff (Eff)
import DOM (DOM)
import Data.Array (singleton)
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Kripke.Kripke (Model, Node, Evaluation, runEvaluation)
import Logics.Intuitionistic.Propositional (exprParser, evaluation)
import Signal.Channel (CHANNEL)
import Sparkle (sparkle)
import Glitter (glit)

combine :: forall err expr. (String -> Either err expr) -> (Evaluation err expr) -> Model -> Node -> String -> Either err Boolean
combine parser eval m n s = do
      expr <- parser s
      evaluate m n expr
      where evaluate = runEvaluation eval

main :: forall eff. Eff ( channel :: CHANNEL, dom :: DOM | eff ) Unit
main = sparkle "model validation" $ combine parse evaluation where
      parse = glit exprParser # map (lmap singleton)