module Logics.Modal.Alethic.Propositional (Expr(..), evaluate) where 

import Prelude

import Data.Array (filter)
import Data.Foldable (and)
import Data.Kripke.Kripke (Atom, Model, Node, testK)

-- A formula of modal propositional logic
data Expr = Taut | Bottom
            | Var Atom | Not Expr
            | Box Expr | Diamond Expr
            | And Expr Expr
            | Or Expr Expr
            | Implies Expr Expr 

evaluate :: Model -> Node -> Expr -> Boolean
evaluate _ _ Taut = true
evaluate _ _ Bottom = false
evaluate m w (Not x) = not $ evaluate m w x
evaluate m w (And x1 x2) = evaluate m w x1 && evaluate m w x2
evaluate m w (Or x1 x2) = evaluate m w x1 || evaluate m w x2
evaluate m w (Implies x1 x2) = (not $ evaluate m w x1) || evaluate m w x2
evaluate { valuation } w (Var v) = testK valuation v w
evaluate m w (Diamond x) = evaluate m w (Not (Box (Not x)))
evaluate m@{ frame: { worlds, relation } } w (Box x) = and $ map (evaluate') accessible
  where evaluate' w' = evaluate m w' x
        accessible = filter (testK relation w) worlds
