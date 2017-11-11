module Data.Kripke.Kripke (Atom, Node, Worlds, Relation, AccessiblePair, KripkeFrame, Valuation, Fact, Model, Evaluation(..), checkModel, runEvaluation, accessible, isFact) where

import Prelude
import Data.Foldable (elem)
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))

type Atom = String

type Node = String

type Worlds = Array Node

type AccessiblePair = Tuple Node Node

type Relation = Array AccessiblePair

type KripkeFrame = { worlds :: Worlds, relation :: Relation }

type Fact = Tuple Atom Node

type Valuation = Array Fact

type Model = { frame :: KripkeFrame, valuation :: Valuation }

data Evaluation err expr = Evaluation (Model -> Either err (Node -> expr -> Boolean))

isFact :: Valuation -> Atom -> Node -> Boolean
isFact v a w = elem (Tuple a w) v

accessible :: Relation -> Node -> Node -> Boolean
accessible r w w' = elem (Tuple w w') r

-- Checks that a model satisfies the laws of a given Evaluation's logic.
checkModel :: forall err a. Evaluation err a -> Model -> Either err Unit
checkModel (Evaluation f) m = void $ f m

-- Runs the evaluation function.
runEvaluation :: forall err expr. Evaluation err expr -> Model -> Node -> expr -> Either err Boolean
runEvaluation (Evaluation f) model world expr = case (f model) of
  Left err -> Left err
  Right eval -> Right (eval world expr)