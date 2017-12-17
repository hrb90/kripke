module Logics.Intuitionistic.Propositional (evaluation, Expr(..), exprParser) where

import Prelude

import Control.Alt ((<|>))
import Control.Lazy (fix)
import Data.Array (filter)

import Data.Foldable (and)
import Data.Functor (voidRight)
import Data.Generic (class Generic)
import Data.Kripke.Kripke (Model, Node, Atom, Evaluation(..), testK)

import Logics.Intuitionistic.Validation (validate)
import Parsing.Token (propToken)
import Text.Parsing.Parser (Parser)

import Text.Parsing.Parser.Expr (Assoc(..), Operator(..), buildExprParser)


-- A formula of intuitionistic propositional logic
data Expr = Top | Bottom
            | Var Atom | Not Expr
            | And Expr Expr
            | Or Expr Expr
            | Implies Expr Expr

derive instance genericExpr :: Generic Expr

-- Parsing!
parens :: forall a. Parser String a -> Parser String a
parens = propToken.parens

reservedOp :: String -> Parser String Unit
reservedOp = propToken.reservedOp

reserved :: String -> Parser String Unit
reserved = propToken.reserved

identifier :: Parser String String
identifier = propToken.identifier

atomParser :: Parser String Expr
atomParser = Var <$> identifier 
            <|> reserved "T" $> Top
            <|> reserved "F" $> Bottom

term :: Parser String Expr -> Parser String Expr
term p = parens p <|> atomParser

exprParser :: Parser String Expr
exprParser = fix allExprs where
  allExprs p = buildExprParser table (term p)
  table =
    [ [ Prefix (reservedOp "!" $> Not) ]
    , [ Infix (reservedOp "||" $> Or) AssocRight ]
    , [ Infix (reservedOp "&&" $> And) AssocRight ]
    , [ Infix (reservedOp "->" $> Implies) AssocRight ] ]

evaluate :: Model -> Node -> Expr -> Boolean
-- T is always true
evaluate _ _ Top = true
-- F is always false
evaluate _ _ Bottom = false
-- !x is syntactic sugar for (x -> F)
-- See below for the semantics of intutionistic implication
evaluate m w (Not x) = evaluate m w (Implies x Bottom)
-- && and || work like you think they do
evaluate m w (And x1 x2) = evaluate m w x1 && evaluate m w x2
evaluate m w (Or x1 x2) = evaluate m w x1 || evaluate m w x2
-- The value of a bare atom is determined by the valuation function in a world
evaluate { valuation } w (Var v) = testK valuation v w
-- x -> y means that, in any accessible world where x is true, y is also true
evaluate m@{ frame: { worlds, relation } } w (Implies x1 x2) = and $ map (evaluate' x2) accessibleSatisfying
  where evaluate' expr world = evaluate m world expr
        accessibleSatisfying = filter ((&&) <$> (testK relation w) <*> (evaluate' x1)) worlds

evaluation :: Evaluation (Array String) Expr
evaluation = Evaluation $ voidRight <$> evaluate <*> validate