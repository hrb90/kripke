module Logics.Modal.Alethic.Propositional (Expr(..), exprParser, evalK, evalT, evalS4, evalS5) where 

import Prelude

import Control.Alt ((<|>))
import Control.Lazy (fix)
import Data.Array (filter)
import Data.Foldable (and)
import Data.Functor (voidRight)
import Data.Generic (class Generic)
import Data.Kripke.Kripke (Model, Node, Atom, Evaluation(..), testK)
import Logics.Modal.Validation (validateK, validateT, validateS4, validateS5)
import Parsing.Token (modalToken)
import Text.Parsing.Parser (Parser)
import Text.Parsing.Parser.Expr (Assoc(..), Operator(..), buildExprParser)

-- A formula of modal propositional logic
data Expr = Top | Bottom
            | Var Atom | Not Expr
            | Box Expr | Diamond Expr
            | And Expr Expr
            | Or Expr Expr
            | Implies Expr Expr 

derive instance genericExpr :: Generic Expr

-- Parsing!
parens :: forall a. Parser String a -> Parser String a
parens = modalToken.parens

reservedOp :: String -> Parser String Unit
reservedOp = modalToken.reservedOp

reserved :: String -> Parser String Unit
reserved = modalToken.reserved

identifier :: Parser String String
identifier = modalToken.identifier

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
    [ [ Prefix (reservedOp "!" $> Not)
      , Prefix (reservedOp "[]" $> Box)
      , Prefix (reservedOp "<>" $> Diamond) ]
    , [ Infix (reservedOp "||" $> Or) AssocRight ]
    , [ Infix (reservedOp "&&" $> And) AssocRight ]
    , [ Infix (reservedOp "->" $> Implies) AssocRight ] ]

evaluate :: Model -> Node -> Expr -> Boolean
evaluate _ _ Top = true
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

evalK :: Evaluation (Array String) Expr
evalK = Evaluation $ voidRight <$> evaluate <*> validateK

evalT :: Evaluation (Array String) Expr
evalT = Evaluation $ voidRight <$> evaluate <*> validateT

evalS4 :: Evaluation (Array String) Expr
evalS4 = Evaluation $ voidRight <$> evaluate <*> validateS4

evalS5 :: Evaluation (Array String) Expr
evalS5 = Evaluation $ voidRight <$> evaluate <*> validateS5