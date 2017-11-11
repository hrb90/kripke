module Logics.Intuitionistic.UniversalQuantification (evaluation, Expr(..), FreeVar, Formula(..)) where

import Prelude

import Data.Array (filter)
import Data.Foldable (and)
import Data.Functor (voidRight)
import Data.Kripke.Kripke (Atom, Evaluation(..), Model, Node, nodeDomain, testK)
import Logics.Intuitionistic.Validation (validate)
import Partial (crash)
import Partial.Unsafe (unsafePartial)

type FreeVar = String

-- A formula of propositional logic, possibly with some free variables.
data Formula = Taut | Bottom | Free FreeVar
            | Var Atom | Not Formula
            | And Formula Formula
            | Or Formula Formula
            | Implies Formula Formula

-- Universal quantification over formulas of propositional logic
data Expr = Formula Formula
            | ForAll FreeVar Expr
 

-- evaluates a Formula without any free variables
evaluateF :: Partial => Model -> Node -> Formula -> Boolean
evaluateF _ _ Taut = true
evaluateF _ _ Bottom = false
evaluateF _ _ (Free _) = crash
evaluateF m w (Not x) = evaluateF m w (Implies x Bottom)
evaluateF m w (And x1 x2) = evaluateF m w x1 && evaluateF m w x2
evaluateF m w (Or x1 x2) = evaluateF m w x1 || evaluateF m w x2
evaluateF { valuation } w (Var v) = testK valuation v w
evaluateF m@{ frame: { worlds, relation } } w (Implies x1 x2) = and $ map (evaluate' x2) accessibleSatisfying
  where evaluate' expr world = evaluateF m world expr
        accessibleSatisfying = filter ((&&) <$> testK relation w <*> evaluate' x1) worlds

bindVarF :: FreeVar -> Atom -> Formula -> Formula
bindVarF free value (Free x)
  | free == x = Var value
  | otherwise = Free x
bindVarF f v (And p1 p2) = And (bindVarF f v p1) (bindVarF f v p2)
bindVarF f v (Or p1 p2) = Or (bindVarF f v p1) (bindVarF f v p2)
bindVarF f v (Implies p1 p2) = Implies (bindVarF f v p1) (bindVarF f v p2)
bindVarF _ _ x = x

bindVar :: FreeVar -> Atom -> Expr -> Expr
bindVar f v (Formula p) = Formula $ bindVarF f v p
bindVar f v (ForAll g x)
  | f == g = bindVar f v x
  | otherwise = ForAll g $ bindVar f v x

getFormula :: Expr -> Formula
getFormula (Formula f) = f
getFormula (ForAll _ x) = getFormula x

getBindings :: Expr -> Array FreeVar
getBindings (Formula _) = []
getBindings (ForAll f x) = [f] <> getBindings x

getFree :: Formula -> Array FreeVar
getFree (Free f) = [f]
getFree (Not p) = getFree p
getFree (And p1 p2) = getFree p1 <> getFree p2
getFree (Or p1 p2) = getFree p1 <> getFree p2
getFree (Implies p1 p2) = getFree p1 <> getFree p2
getFree _ = []

evaluate :: Model -> Node -> Expr -> Boolean
evaluate m w (Formula p) = unsafePartial $ evaluateF m w p
evaluate m@{ domain, frame: { worlds, relation } } w x@(ForAll var _) = 
  and $ do
    w' <- filter (testK relation w) worlds
    v <- nodeDomain domain w'
    pure $ evaluate m w $ bindVar var v x

evaluation :: Evaluation (Array String) Expr
evaluation = Evaluation $ voidRight <$> evaluate <*> validate