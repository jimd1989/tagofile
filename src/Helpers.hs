module Helpers where

import Control.Monad ((<=<))
import Data.Functor (($>))

head' ∷ [a] → Maybe a
head' []      = Nothing
head' (α : ω) = Just α

tail' ∷ [a] → Maybe [a]
tail' α = head' α $> tail α

note ∷ a → Maybe b → Either a b
note α Nothing  = Left α
note _ (Just β) = Right β

g ◁ f = (<$>) g . f
infixr 9 ◁

g ◀ f = g <=< f
infixr 1 ◀

f ⊙ m = f <$> m
infixl 4 ⊙

α ◇ ω = α <> ω
infixr 5 ◇
