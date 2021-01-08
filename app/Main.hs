module Main where

import Args (parseArgs)

main ∷ IO ()
main = parseArgs >> return ()
