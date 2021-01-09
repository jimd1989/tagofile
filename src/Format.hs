module Format (Format, Matcher, format) where

-- Concerned with reading the format' string into an array of Matcher types,
-- which tell the program what type of input to expect from the filenames,
-- what delimiters to look out for, etc.
--
-- I am not concerned with performance and am just appending to lists. Deal.

import Data.Set as S (Set, fromList, member)
import Helpers ((⊙), (◇), note, tail', toIO)

data Matcher = Blank | Literal String | Num | Until (Maybe Char) Int
  deriving (Show, Eq)

type Format = [Matcher]

formats ∷ Set Char
formats = fromList "ndtaAbYG"

num ∷ Char → Bool
num α = or $ (α ==) ⊙ ['n', 'd']

literal ∷ Char → Matcher
literal α = Literal [α]

isLiteral ∷ Matcher → Bool
isLiteral (Literal _) = True
isLiteral _           = False

until' ∷ String → Matcher
until' [] = Until Nothing 0
until' (α : ω) = Until (Just α) (length $ filter (== α) ω)

add ∷ Matcher → String → Matcher
add (Literal α) ω = Literal $ α ◇ ω
add α _           = α

format' ∷ [Matcher] → Matcher → String → Format
format' ms m []                 = ms ◇ [m]
format' ms m ('{' : α : '}' : ω)
  | member α formats && num α = format' (ms ◇ [m]) Num ω
  | member α formats          = format' (ms ◇ [m]) (until' ω) ω
  | isLiteral m               = format' ms (add m $ "{" ◇ [α] ◇ "}") ω
  | otherwise                 = format' (ms ◇ [m]) (literal α) ω
format' ms m (α : ω)
  | isLiteral m               = format' ms (add m [α]) ω
  | otherwise                 = format' (ms ◇ [m]) (literal α) ω

format ∷ String → IO Format
format = toIO . note "internal error" . tail' . format' [] Blank
