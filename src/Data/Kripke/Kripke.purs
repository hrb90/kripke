module Data.Kripke.Kripke (Atom, Node, Worlds, Relation, AccessiblePair, KripkeFrame, Valuation, Fact, Model, Evaluation(..), checkModel, runEvaluation, testK) where

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

-- Specializes to testing that an atom is part of the valuation of a node
-- Or to testing that the second argument is accessible from the first
testK :: forall a b. Eq a => Eq b => Array (Tuple a b) -> a -> b -> Boolean
testK s x y = elem (Tuple x y) s

-- Checks that a model satisfies the laws of a given Evaluation's logic.
checkModel :: forall err a. Evaluation err a -> Model -> Either err Unit
checkModel (Evaluation f) m = void $ f m

-- Runs the evaluation function.
runEvaluation :: forall err expr. Evaluation err expr -> Model -> Node -> expr -> Either err Boolean
runEvaluation (Evaluation f) model world expr = case (f model) of
  Left err -> Left err
  Right eval -> Right (eval world expr)