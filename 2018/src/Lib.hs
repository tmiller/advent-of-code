module Lib ( (|>)
           , (||>)
           , Parser
           , ParseErrorBundle
           , eitherPE
           , integer
           ) where

import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char as MC
import qualified Text.Megaparsec.Char.Lexer as L

infixl 0 |>
infixl 4 ||>

(|>) :: a -> (a -> b) -> b
(|>) = flip ($)

(||>) :: Functor f => f a -> (a -> b) -> f b
(||>) = flip (<$>)

-- Parsers

type Parser = Parsec Void String
type ParseErrorBundle = ParseError (Token String) Void

integer :: Parser Integer
integer = L.signed space (L.lexeme space L.decimal)

eitherPE :: (ShowToken t, ShowErrorComponent e, Ord t) =>
  (b -> String) -> Either (ParseError t e) b -> String
eitherPE = either parseErrorPretty
