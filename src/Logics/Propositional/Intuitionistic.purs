module Logics.Propositional.Intuitionistic (evaluation, Expr(..)) where

import Prelude

import Data.Array (filter)
import Data.Either (Either(..))
import Data.Foldable (and, elem) 
import Data.Kripke.Kripke (Model, Node, Atom, Evaluation(..))
import Data.Tuple (Tuple(..))


-- A formula of intuitionistic propositional logic
data Expr = Taut | Bottom
            | Var Atom | Not Expr
            | And Expr Expr
            | Or Expr Expr
            | Implies Expr Expr 


evaluate :: Model -> Node -> Expr -> Boolean
evaluate _ _ Taut = true
evaluate _ _ Bottom = false
evaluate m w (Not x) = evaluate m w (Implies x Bottom)
evaluate m w (And x1 x2) = evaluate m w x1 && evaluate m w x2
evaluate m w (Or x1 x2) = evaluate m w x1 || evaluate m w x2
evaluate { valuation } w (Var v) = elem (Tuple v w) valuation
evaluate m@{ frame: { worlds, relation } } w (Implies x1 x2) = and $ map (evaluate' x2) accessibleSatisfying
  where evaluate' exp world = evaluate m world exp
        accessibleSatisfying = filter (\w' -> (evaluate' x1 w') && (elem (Tuple w w') relation)) worlds

evaluation :: forall a. Evaluation a Expr
evaluation = Evaluation neverFail
  where neverFail m = Right $ \w expr -> evaluate m w expr