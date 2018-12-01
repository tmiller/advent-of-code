module Lib ( (|>)
           , (||>)
           ) where

infixl 0 |>
infixl 4 ||>

(|>) :: a -> (a -> b) -> b
(|>) = flip ($)

(||>) :: Functor f => f a -> (a -> b) -> f b
(||>) = flip (<$>)
