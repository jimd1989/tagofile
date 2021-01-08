module Args (ParsedArgs, parseArgs) where

import Control.Arrow ((|||))
import Data.Function ((&), const)
import Data.Functor (($>))
import Data.Map as M
import Data.Tuple (swap)
import Helpers ((⊙), (◁), (◇), note, tail')
import System.Environment (getArgs)
import System.IO.Error (userError)

type Arg = String
type Args = [String]
type ArgVal = String
type GlobalTags = Map Arg ArgVal

globalTags ∷ GlobalTags
globalTags = fromList [("-a", ""),("-A", ""),("-b", ""),("-G", ""),("-Y", "")]

set ∷ Arg → ArgVal → GlobalTags → Either String GlobalTags
set k v gt = M.lookup k gt $> M.adjust (const v) k gt & note (err k)
  where err k = k ◇ " is not a valid tag"

isArg ∷ Arg → Bool
isArg ""         = False
isArg ('-' : ω)  = True
isArg α          = False

data ParsedArgs = ParsedArgs {
  tags ∷ GlobalTags,
  format ∷ ArgVal,
  files ∷ [ArgVal]
} deriving Show

noDashes ∷ GlobalTags → Maybe GlobalTags
noDashes = (M.fromList . (⊙) swap) ◁ mapM (sequence . tail' ◁ swap) . M.toList

parsedArgs ∷ ArgVal → [ArgVal] → GlobalTags → Maybe ParsedArgs
parsedArgs format files = (\tags → ParsedArgs {tags, format, files}) ◁ noDashes

parse ∷ Args → GlobalTags → Either String ParsedArgs
parse [] gt = Left "no format string / files specified"
parse (α : β : ω) gt
  | isArg α && isArg β  = Left $ α ◇ " needs value"
  | isArg α             = set α β gt >>= parse ω
  | otherwise           = note "internal error" $ parsedArgs α (β : ω) gt

parseArgs ∷ IO ParsedArgs
parseArgs = (ioError . userError ||| pure) . flip parse globalTags =<< getArgs
