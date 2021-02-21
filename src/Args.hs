module Args (ParsedArgs, files, fmt, parseArgs, tags) where

-- Concerned with parsing command line arguments into a struct ParsedArgs,
-- which contains any global tag values for artist, genre, etc, as well
-- as the format string and files to be tagged.

import Data.Function ((&), const)
import Data.Functor (($>))
import Data.Map as M (Map, adjust, fromList, lookup, toList)
import Data.Tuple (swap)
import Helpers ((⊙), (◁), (◇), note, tail', toIO)
import System.Environment (getArgs)

type Arg = String
type Args = [String]
type ArgVal = String
type GlobalTags = Map Arg ArgVal

globalTags ∷ GlobalTags
globalTags = fromList $ (,"") ⊙ ["-a", "-A", "-b", "-G", "-Y"]

set ∷ Arg → ArgVal → GlobalTags → Either String GlobalTags
set k v gt = M.lookup k gt $> adjust (const v) k gt & note (err k)
  where err k = k ◇ " is not a valid tag"

isArg ∷ Arg → Bool
isArg ""         = False
isArg ('-' : ω)  = True
isArg α          = False

data ParsedArgs = ParsedArgs {
  tags ∷ GlobalTags,
  fmt ∷ ArgVal,
  files ∷ [ArgVal]
} deriving Show

noDash ∷ GlobalTags → Maybe GlobalTags
noDash = (fromList . (⊙) swap) ◁ mapM (sequence . tail' ◁ swap) . toList

parsedArgs ∷ ArgVal → [ArgVal] → GlobalTags → Maybe ParsedArgs
parsedArgs fmt files = (\tags → ParsedArgs {tags, fmt, files}) ◁ noDash

parse ∷ Args → GlobalTags → Either String ParsedArgs
parse [] gt             = Left "no _format string / files specified"
parse (α : β : ω) gt
  | isArg α && isArg β  = Left $ α ◇ " needs value"
  | isArg α             = set α β gt >>= parse ω
  | otherwise           = note "internal error" $ parsedArgs α (β : ω) gt
parse _ _               = Left "malformed arguments"

parseArgs ∷ IO ParsedArgs
parseArgs = toIO . flip parse globalTags =<< getArgs
