module Matchers where

import Control.Monad (liftM2)
import Data.List.Split (splitOn)
import Helpers ((◁), drop', flip2, slice, tail', take')

type Tag = String
type Delimiter = String
type Remainder = String
type Format = String
type Filename = String
type Length = Int
type Count = Int
data Matcher = Blank
             | Txt Delimiter Length
             | Num Tag
             | Field Tag
  deriving (Show, Eq)

delimiterCount ∷ Delimiter → Filename → Int
delimiterCount α = pred . length . splitOn α

matcherCount ∷ Matcher → [Matcher] → Int
matcherCount α = length . filter (α ==)

txtCount ∷ Matcher → [Matcher] → Filename → Maybe Count
txtCount t@(Txt α _) ms ω = pure $ (delimiterCount α ω) - (matcherCount t ms)
txtCount _ _ _            = Nothing

txtLength ∷ Matcher → Count → Filename → Maybe Length
txtLength t@(Txt α n) 0 ω
  | take n ω == α  = pure n
  | otherwise      = succ ◁ txtLength t 0 =<< tail' ω
txtLength t@(Txt α n) c ω
  | take n ω == α  = (+ n) ◁ txtLength t (pred c) =<< drop' n ω
  | otherwise      = succ ◁ txtLength t c =<< tail' ω
txtLength _ _ _    = Nothing

txtMatch ∷ Matcher → [Matcher] → Filename → Maybe (String, Remainder)
txtMatch t ts α = flip slice α =<< flip2 txtLength t α =<< txtCount t ts α

-- For (Num | Field), Txt, Ω ...
-- 1. Map across Ω ... and txtCount if any delimiters are the same as Txt
-- 2. Map across remaining string and txtCount occurances of Txt
-- 3. Subtract SAME COUNT from DELIMITER COUNT. Use difference when matching for Txt
-- 4. Every time delimiter is entxtCountered, subtract the total
-- 5. When the total is exactly 0, return the token
-- 6. Recurse with Ω ...
--
-- Example: "{a} - {A}.mp3"
-- Example: "The Artist - So Great - The Artist's Hits.mp3"
-- Matchers: Field "a", Txt " - ", Field "A", Txt ".mp3"
-- For Field "a", Txt "-", Ω ...
-- SAME COUNT = 0
-- DELIMITER COUNT = 2
-- 
--
