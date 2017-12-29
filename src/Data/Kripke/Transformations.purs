module Data.Kripke.Transformations where
  
import Prelude

import Data.Array (filter)
import Data.Foldable (elem)
import Data.Kripke.Kripke (KripkeFrame, testK)
import Data.Kripke.Validation (isTransitive)
import Data.Tuple (Tuple(..))

reflexiveClosure :: KripkeFrame -> KripkeFrame
reflexiveClosure { worlds, relation } = { worlds, relation: relation <> extra }
  where extra = filter (not $ flip elem relation) (map (\w -> Tuple w w) worlds)

transitiveClosure :: KripkeFrame -> KripkeFrame
transitiveClosure frame@{ worlds, relation }
  | isTransitive frame = frame
  | otherwise = transitiveClosure $ { worlds, relation: relation <> extra }
      where extra = filter (not $ flip elem relation) $ do
              (Tuple u v) <- relation
              w <- filter (testK relation v) worlds
              pure $ Tuple u w