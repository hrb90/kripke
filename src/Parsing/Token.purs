module Parsing.Token (propToken, modalToken) where
  
import Control.Alt ((<|>))

import Data.Char.Unicode (isLower)
import Text.Parsing.Parser.Token (LanguageDef, GenLanguageDef(..), TokenParser, makeTokenParser)
import Text.Parsing.Parser.String (satisfy, char, oneOf)

propositional :: LanguageDef
propositional = LanguageDef
  { commentStart: ""
  , commentEnd: ""
  , commentLine: ""
  , nestedComments: false
  , opStart: oneOf ['-']
  , opLetter: oneOf ['=', '|']
  , identStart: satisfy isLower
  , identLetter: satisfy isLower <|> char '\''
  , reservedNames: ["T", "F"]
  , reservedOpNames: ["->", "&&", "||", "!"]
  , caseSensitive: true
}

propToken :: TokenParser
propToken = makeTokenParser propositional

modal :: LanguageDef
modal = LanguageDef
  { commentStart: ""
  , commentEnd: ""
  , commentLine: ""
  , nestedComments: false
  , opStart: oneOf ['-']
  , opLetter: oneOf ['=', '|']
  , identStart: satisfy isLower
  , identLetter: satisfy isLower <|> char '\''
  , reservedNames: ["T", "F"]
  , reservedOpNames: ["->", "&&", "||", "!", "<>", "[]"]
  , caseSensitive: true
}

modalToken :: TokenParser
modalToken = makeTokenParser modal