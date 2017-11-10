module Kripke where
  
import Prelude

import Data.Array (filter)
import Data.Foldable (and, elem)
import Data.Maybe (Maybe(..))

type Atom = String

data Expr = BooleanLiteral Boolean
            | Var Atom | Not Expr
            | And Expr Expr | Or Expr Expr | Implies Expr Expr 

type Node = String

type Worlds = Array Node

type Relation = Node -> Node -> Maybe Ordering

leq :: Maybe Ordering -> Boolean
leq (Just LT) = true
leq (Just EQ) = true
leq _ = false

type Valuation = Node -> Array Atom

type KripkeFrame = { worlds :: Worlds, relation :: Relation, valuation :: Valuation }

evaluate :: KripkeFrame -> Node -> Expr -> Boolean
evaluate _ _ (BooleanLiteral b) = b
evaluate m w (Not x) = not $ evaluate m w x
evaluate m w (And x1 x2) = evaluate m w x1 && evaluate m w x2
evaluate m w (Or x1 x2) = evaluate m w x1 || evaluate m w x2
evaluate m@{ worlds, relation } w (Implies x1 x2) = and $ map evaluate' accessibleWorlds
  where evaluate' w' = not $ evaluate m w' x1 || evaluate m w x2
        accessibleWorlds = filter (\w' -> leq $ relation w w') worlds
evaluate { valuation } w (Var v) = elem v $ valuation w