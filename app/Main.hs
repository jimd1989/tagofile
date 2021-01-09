module Main where

import Args (fmt, parseArgs)
import Format (format)

main ∷ IO ()
main =  putStrLn . show =<< format . fmt =<< parseArgs
