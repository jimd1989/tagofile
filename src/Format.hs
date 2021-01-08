module Format where

import Data.Set (Set, fromList)

data Matcher = Literal | Num | Text

digits ∷ Set Char
digits = fromList "0123456789"

formats ∷ Set Char
formats = fromList "aAbYG"
