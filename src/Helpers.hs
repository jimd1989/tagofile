module Helpers where

import Control.Arrow ((|||))
import Control.Monad ((<=<))
import Data.Functor (($>), (<&>))
import System.IO.Error (userError)

head' ∷ [a] → Maybe a
head' []      = Nothing
head' (α : ω) = Just α

tail' ∷ [a] → Maybe [a]
tail' α = head' α $> tail α

note ∷ a → Maybe b → Either a b
note α Nothing  = Left α
note _ (Just β) = Right β

toIO ∷ Either String a → IO a
toIO = ioError . userError ||| pure

g ◁ f = (<$>) g . f
infixr 9 ◁

g ◀ f = g <=< f
infixr 1 ◀

f ⊙ m = f <$> m
infixl 4 ⊙

m ⊖ f = m <&> f
infixl 1 ⊖

f ● m = f <*> m
infixl 4 ●

α ◇ ω = α <> ω
infixr 5 ◇

α ◆ ω = α <> [ω]
infixr 5 ◆
