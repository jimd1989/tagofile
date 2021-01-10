module Match where

import Data.Char (isDigit)
import Data.Map as M (Map, fromList)
import Helpers ((◆))

type Tag = Char
type Delimiter = Maybe Char
data Matcher = Blank
             | Txt String
             | Num Tag Delimiter Int
             | Until Tag Delimiter Int
             deriving (Show, Eq)

type Progress = (String, String)
type Matched = Map String String

txt ∷ Char → Matcher
txt α = Txt [α]

isTxt ∷ Matcher → Bool
isTxt (Txt _) = True
isTxt _           = False

num ∷ Tag → String → Matcher
num t []      = Num t Nothing 0
num t (α : ω) = Num t (Just α) (length $ filter (α ==) ω)

until' ∷ Tag → String → Matcher
until' t []      = Until t Nothing 0
until' t (α : ω) = Until t (Just α) (length $ filter (α ==) ω)

matchTxt ∷ String → String → Maybe String
matchTxt [] α                     = Just α
matchTxt (α : ω) []               = Nothing
matchTxt (α : ω) (β : ψ) | α == β = matchTxt ω ψ
matchTxt _ _                      = Nothing

matchUntil ∷ Delimiter → Int → Progress → Maybe Progress
matchUntil Nothing 0 ([], ψ)       = Just ([], ψ)
matchUntil Nothing 0 ((α : ω), ψ)  = matchUntil Nothing 0 (ω, ψ ◆ α)
matchUntil (Just d) n ((α : ω), ψ)
  | d == α && n == 0               = Just (ω, ψ)
  | d == α                         = matchUntil (Just d) (pred n) (ω, ψ ◆ α)
  | otherwise                      = matchUntil (Just d) n (ω, ψ ◆ α)
matchUntil _ _ _                   = Nothing

