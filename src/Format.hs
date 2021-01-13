module Format where

-- Concerned with reading the format string into an array of Matcher types,
-- which tell the program what type of input to expect from the filenames,
-- what delimiters to look out for, etc.

import Control.Monad (foldM)
import Data.Set as S (Set, fromList, member)
import Helpers ((◀), (◁), (◇), note, tail', toIO)
import Matchers (Matcher(..))

type Format = [Matcher]

fields ∷ Set Char
fields = fromList "taAbG"

numFields ∷ Set Char
numFields = fromList "ndY"

isTxt ∷ Matcher → Bool
isTxt (Txt _) = True
isTxt _       = False

txtAdd ∷ Matcher → String → Matcher
txtAdd (Txt α) ω = Txt $ α ◇ ω
txtAdd α _       = α

format' ∷ [Matcher] → Matcher → String → Format
format' ms m []        = reverse $ m : ms
format' ms m ('{' : α : '}' : ω)
  | member α numFields = format' (m : ms) (Num $ pure α) ω 
  | member α fields    = format' (m : ms) (Field $ pure α) ω
  | isTxt m            = format' ms (txtAdd m $ "{" ◇ [α] ◇ "}") ω
  | otherwise          = format' (m : ms) (Txt $ pure α) ω
format' ms m (α : ω)
  | isTxt m            = format' ms (txtAdd m $ pure α) ω
  | otherwise          = format' (m : ms) (Txt $ pure α) ω

ambiguity' ∷ [Matcher] → Matcher → Either String [Matcher]
ambiguity' [] α                                = Right [α]
ambiguity' (β@(Txt _ ) : ω) α  | not $ isTxt α = Right (α : β : ω)
ambiguity' (β : ω) α@(Txt _)                   = Right (α : β : ω)
ambiguity' _ _                                 = Left "ambiguous format string"

ambiguity ∷ [Matcher] → Either String [Matcher]
ambiguity = reverse ◁ foldM ambiguity' []

format ∷ String → IO Format
format = toIO . (ambiguity ◀ note "internal error" . tail' . format' [] Blank)
