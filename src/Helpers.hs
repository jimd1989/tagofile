module Helpers where

import Control.Arrow ((|||))
import Control.Monad ((<=<), liftM2)
import Data.Bitraversable (bisequence)
import Data.Functor (($>), (<&>))
import System.IO.Error (userError)

head' ∷ [a] → Maybe a
head' []      = Nothing
head' (α : ω) = Just α

tail' ∷ [a] → Maybe [a]
tail' α = head' α $> tail α

drop' ∷ Int → [a] → Maybe [a]
drop' n = tail' . drop (pred n)

take' ∷ Int → [a] → Maybe [a]
take' n = liftM2 ($>) (drop' n) (take n)

slice ∷ Int → [a] → Maybe ([a], [a])
slice n = bisequence . liftM2 (,) (take' n) (drop' n)

flip2 ∷ (a → b → c → d) → (a → c → b → d)
flip2 f α = (flip $ f α)

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
