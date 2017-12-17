module TestHelpers (glitter, glitter') where

import Prelude

import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Generic (class Generic)
import Text.Parsing.Parser (Parser, parseErrorMessage, runParser)

glitter :: forall a b. Generic b => Parser String a -> (a -> b) -> String -> Either String b
glitter parser evaluate = parser
                            # map evaluate
                            # flip runParser
                            # (map) (lmap parseErrorMessage)

glitter' :: forall a. Generic a => Parser String a -> String -> Either String a
glitter' = flip glitter id
