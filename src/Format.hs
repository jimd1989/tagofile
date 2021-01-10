module Format (Format, format) where

-- Concerned with reading the format' string into an array of Matcher types,
-- which tell the program what type of input to expect from the filenames,
-- what delimiters to look out for, etc.
--
-- I am not concerned with performance and am just appending to lists. Deal.

import Data.Set as S (Set, fromList, member)
import Helpers ((⊙), (◇), (◆), note, tail', toIO)
import Match (Matcher(..), isTxt, num, txt, until')

type Format = [Matcher]

formats ∷ Set Char
formats = fromList "ndtaAbYG"

isNum ∷ Char → Bool
isNum α = or $ (α ==) ⊙ ['n', 'd']

add ∷ Matcher → String → Matcher
add (Txt α) ω = Txt $ α ◇ ω
add α _       = α

format' ∷ [Matcher] → Matcher → String → Format
format' ms m []                        = ms ◆ m
format' ms m ('{' : α : '}' : β : ω)
  | member α formats && isNum α = format' (ms ◆ m) (num α (β : ω)) ω
  | member α formats            = format' (ms ◆ m) (until' α (β : ω)) ω
  | isTxt m                     = format' ms (add m $ "{" ◇ [α] ◇ "}") (β : ω)
  | otherwise                   = format' (ms ◆ m) (txt α) (β : ω)
format' ms m (α : ω)
  | isTxt m                     = format' ms (add m [α]) ω
  | otherwise                   = format' (ms ◆ m) (txt α) ω

format ∷ String → IO Format
format = toIO . note "internal error" . tail' . format' [] Blank
