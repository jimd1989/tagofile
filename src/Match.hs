module Match where

import Control.Arrow ((***))
import Data.Char (isDigit)
import Data.Functor (($>))
import Data.Map as M (Map, fromList, insert)
import Helpers ((◆), (⊖))
import Text.Read (readMaybe)

type Tag = Char
type Delimiter = Maybe Char
data Matcher = Blank
             | Txt String
             | Num Tag Delimiter
             | Until Tag Delimiter Int
             deriving (Show, Eq)

type Progress = (String, String)
type Matched = Map String String

txt ∷ Char → Matcher
txt α = Txt [α]

isTxt ∷ Matcher → Bool
isTxt (Txt _) = True
isTxt _           = False

numStr ∷ String → Maybe String
numStr α = (readMaybe α ∷ Maybe Int) $> α

delimiterCount ∷ Char → String → Int
delimiterCount α = length . filter (α ==)

total ∷ Delimiter → Int → String → Int
total Nothing _ _  = 0
total (Just α) n β = (delimiterCount α β) - n - 1

num ∷ Tag → String → Matcher
num t []      = Num t Nothing
num t (α : ω) = Num t (Just α)

until' ∷ Tag → String → Matcher
until' t []      = Until t Nothing 0
until' t (α : ω) = Until t (Just α) (delimiterCount α ω)

matchTxt ∷ String → String → Maybe String
matchTxt [] α                     = Just α
matchTxt (α : ω) []               = Nothing
matchTxt (α : ω) (β : ψ) | α == β = matchTxt ω ψ
matchTxt _ _                      = Nothing

matchNum ∷ Delimiter → Progress → Maybe Progress
matchNum Nothing ([], ψ)         = sequence ([], numStr ψ)
matchNum Nothing ((α : ω), ψ)    = matchNum Nothing (ω, ψ ◆ α)
matchNum (Just d) ((α : ω), ψ)
  | d == α                       = sequence (ω, numStr ψ)
  | otherwise                    = matchNum (Just d) (ω, ψ ◆ α)
matchNum _ _                     = Nothing

matchUntil ∷ Delimiter → Int → Progress → Maybe Progress
matchUntil Nothing 0 ([], ψ)       = Just ([], ψ)
matchUntil Nothing 0 ((α : ω), ψ)  = matchUntil Nothing 0 (ω, ψ ◆ α)
matchUntil (Just d) n ((α : ω), ψ)
  | n < 0                          = Nothing
  | d == α && n == 0               = Just (ω, ψ)
  | d == α                         = matchUntil (Just d) (pred n) (ω, ψ ◆ α)
  | otherwise                      = matchUntil (Just d) n (ω, ψ ◆ α)
matchUntil _ _ _                   = Nothing

add ∷ Matched → Char → String → Matched
add m k v = insert (pure k) v m

match' ∷ Matched → Matcher → String → Maybe (String, Matched)
match' m (Txt α) β       = matchTxt α β ⊖ (,m)
match' m (Num t d) β     = matchNum d (β, []) ⊖ (id *** add m t)
match' m (Until t d n) β = matchUntil d (total d n β) (β, []) ⊖ (id *** add m t)

match ∷ [Matcher] → (String, Matched) → Maybe Matched
match [] ([], m)     = Just m
match (α : ω) (s, m) = match' m α s >>= match ω
