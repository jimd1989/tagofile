module Main where

import Args (files, fmt, parseArgs, tags)
import Format (format)
import Helpers ((⊙))
import Match (match)

main ∷ IO ()
main = do
  args        ← parseArgs
  fileFormat  ← format $ fmt args
  parsedFiles ← pure $ match fileFormat ⊙ (files args)
  mapM_ (putStrLn . show) parsedFiles
