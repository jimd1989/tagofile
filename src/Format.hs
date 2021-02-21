module Format where

-- Concerned with reading the format string into an array of Matcher types,
-- which tell the program what type of input to expect from the filenames,
-- what delimiters to look out for, etc.

import Control.Monad (foldM)
import Data.Set as S (Set, fromList, member)
import Helpers ((◀), (◁), (◇), flip2, note, tail', toIO)
import Matchers (Matcher(..))

type Format = [Matcher]

fields ∷ Set Char
fields = fromList "taAbG"

numFields ∷ Set Char
numFields = fromList "ndY"

isTxt ∷ Matcher → Bool
isTxt (Txt _ _) = True
isTxt _         = False

txtAdd ∷ Matcher → String → Either String Matcher
txtAdd (Txt α n) ω = pure $ Txt (α ◇ ω) (n + (length ω))
txtAdd α _         = Left "internal delimiter processing error"

wrap ∷ Char → String
wrap α = '{' : α : '}' : []

format' ∷ [Matcher] → String → Matcher → Either String Format
format' ms [] m        = pure . reverse $ m : ms
format' ms ('{' : α : '}' : ω) m
  | member α numFields = format' (m : ms) ω (Num $ pure α)
  | member α fields    = format' (m : ms) ω (Field $ pure α)
  | isTxt m            = txtAdd m (wrap α) >>= format' ms ω
  | otherwise          = format' (m : ms) ω (Txt (wrap α) 3)
format' ms (α : ω) m
  | isTxt m            = txtAdd m [α] >>= format' ms ω
  | otherwise          = format' (m : ms) ω (Txt [α] 1)

ambiguity' ∷ [Matcher] → Matcher → Either String [Matcher]
ambiguity' [] α                                 = Right [α]
ambiguity' (β@(Txt _ _) : ω) α  | not $ isTxt α = Right (α : β : ω)
ambiguity' (β : ω) α@(Txt _ _)                  = Right (α : β : ω)
ambiguity' _ _                                  = Left "ambiguous format string"

ambiguity ∷ [Matcher] → Either String [Matcher]
ambiguity = reverse ◁ foldM ambiguity' []

format ∷ String → IO Format
format = toIO . (ambiguity ◀ err ◀ flip2 format' [] Blank)
  where err = note "internal format string processing error" . tail'
