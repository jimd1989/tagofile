module Match where

import Data.Char (isDigit)

type Tag = Char
type Delimiter = Maybe Char
data Matcher = Blank
             | Literal String
             | Num Tag Delimiter Int
             | Until Tag Delimiter Int
             deriving (Show, Eq)

type Progress = (Matcher, String)

literal ∷ Char → Matcher
literal α = Literal [α]

isLiteral ∷ Matcher → Bool
isLiteral (Literal _) = True
isLiteral _           = False

num ∷ Tag → String → Matcher
num t []      = Num t Nothing 0
num t (α : ω) = Num t (Just α) (length $ filter (== α) ω)

until' ∷ Tag → String → Matcher
until' t []      = Until t Nothing 0
until' t (α : ω) = Until t (Just α) (length $ filter (== α) ω)

matchLiteral ∷ Progress → Maybe String
matchLiteral (Literal [], α)                     = Just α
matchLiteral (Literal (α : ω), [])               = Nothing
matchLiteral (Literal (α : ω), (β : ψ)) | α == β = matchLiteral (Literal ω, ψ)
matchLiteral _                                   = Nothing
