module Helpers where

safeHead ∷ [a] → Maybe a
safeHead []      = Nothing
safeHead (α : ω) = Just α

note ∷ a → Maybe b → Either a b
note α Nothing  = Left α
note _ (Just β) = Right β

α ◇ ω = α <> ω
