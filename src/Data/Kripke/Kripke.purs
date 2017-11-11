module Data.Kripke.Kripke (Atom, Node, Worlds, Relation, AccessiblePair, KripkeFrame, Valuation, Fact, Model, Evaluation(..), checkModel, runEvaluation) where
  
import Prelude
import Data.Tuple (Tuple)
import Data.Either (Either(..))

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

-- Checks that a model satisfies the laws of a given Evaluation's logic.
checkModel :: forall err a. Evaluation err a -> Model -> Either err Unit
checkModel (Evaluation f) m = void $ f m

-- Runs the evaluation function.
runEvaluation :: forall err expr. Evaluation err expr -> Model -> Node -> expr -> Either err Boolean
runEvaluation (Evaluation f) model world expr = case (f model) of
  Left err -> Left err
  Right eval -> Right (eval world expr)