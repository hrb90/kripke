module Logics.Intuitionistic.Propositional (evaluation, Expr(..)) where

import Prelude

import Data.Array (filter)
import Data.Foldable (and)
import Data.Functor (voidRight)
import Data.Kripke.Kripke ( Model
                          , Node
                          , Atom
                          , Evaluation(..)
                          , testK )
import Logics.Intuitionistic.Validation (validate)


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
evaluate { valuation } w (Var v) = testK valuation v w
evaluate m@{ frame: { worlds, relation } } w (Implies x1 x2) = and $ map (evaluate' x2) accessibleSatisfying
  where evaluate' expr world = evaluate m world expr
        accessibleSatisfying = filter ((&&) <$> (testK relation w) <*> (evaluate' x1)) worlds

evaluation :: Evaluation (Array String) Expr
evaluation = Evaluation $ voidRight <$> evaluate <*> validate