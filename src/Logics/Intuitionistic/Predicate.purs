module Logics.Intuitionistic.Predicate (evaluation, Expr(..), FreeVar, Pred(..)) where

import Prelude

import Data.Array (filter)
import Data.Foldable (and, or)
import Data.Functor (voidRight)
import Data.Kripke.Kripke (Atom, Evaluation(..), Model, Node, nodeDomain, testK)
import Logics.Intuitionistic.Validation (validate)
import Partial (crash)
import Partial.Unsafe (unsafePartial)

type FreeVar = String

-- A predicate
data Pred = Taut | Bottom | Free FreeVar
            | Var Atom | Not Pred
            | And Pred Pred
            | Or Pred Pred
            | Implies Pred Pred


data Expr = Predicate Pred
            | Exists FreeVar Expr
            | ForAll FreeVar Expr
 

-- evaluates a predicate without any free variables
evaluateP :: Partial => Model -> Node -> Pred -> Boolean
evaluateP _ _ Taut = true
evaluateP _ _ Bottom = false
evaluateP _ _ (Free _) = crash
evaluateP m w (Not x) = evaluateP m w (Implies x Bottom)
evaluateP m w (And x1 x2) = evaluateP m w x1 && evaluateP m w x2
evaluateP m w (Or x1 x2) = evaluateP m w x1 || evaluateP m w x2
evaluateP { valuation } w (Var v) = testK valuation v w
evaluateP m@{ frame: { worlds, relation } } w (Implies x1 x2) = and $ map (evaluate' x2) accessibleSatisfying
  where evaluate' expr world = evaluateP m world expr
        accessibleSatisfying = filter ((&&) <$> testK relation w <*> evaluate' x1) worlds

bindVarP :: FreeVar -> Atom -> Pred -> Pred
bindVarP free value (Free x)
  | free == x = Var value
  | otherwise = Free x
bindVarP f v (And p1 p2) = And (bindVarP f v p1) (bindVarP f v p2)
bindVarP f v (Or p1 p2) = Or (bindVarP f v p1) (bindVarP f v p2)
bindVarP f v (Implies p1 p2) = Implies (bindVarP f v p1) (bindVarP f v p2)
bindVarP _ _ x = x

bindVar :: FreeVar -> Atom -> Expr -> Expr
bindVar f v (Predicate p) = Predicate $ bindVarP f v p
bindVar f v (Exists g x)
  | f == g = bindVar f v x
  | otherwise = Exists g $ bindVar f v x
bindVar f v (ForAll g x)
  | f == g = bindVar f v x
  | otherwise = ForAll g $ bindVar f v x

evaluate :: Model -> Node -> Expr -> Boolean
evaluate m w (Predicate p) = unsafePartial $ evaluateP m w p
evaluate m@{ domain } w x@(Exists var _) = 
  or $ map (\v -> evaluate m w $ bindVar var v x) (nodeDomain domain w)
evaluate m@{ domain, frame: { worlds, relation } } w x@(ForAll var _) = 
  and $ do
    w' <- filter (testK relation w) worlds
    v <- nodeDomain domain w'
    pure $ evaluate m w $ bindVar var v x

evaluation :: Evaluation (Array String) Expr
evaluation = Evaluation $ voidRight <$> evaluate <*> validate