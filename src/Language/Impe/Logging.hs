module Language.Impe.Logging
  ( Log (..),
    Tag (..),
    log,
  )
where

import Polysemy
import Polysemy.Output
import Text.Printf
import Prelude hiding (log)

{-
# Logging
-}

{-
## Data
-}

data Log = Log Tag String

data Tag
  = Tag_Debug
  | Tag_Warning
  | Tag_Error
  | Tag_Output
  deriving (Eq)

instance Show Log where
  show (Log tag msg) = case tag of
    Tag_Debug -> printf "[info] %s\n" msg
    Tag_Warning -> printf "[warning] %s\n" msg
    Tag_Error -> printf "[error]\n%s\n" msg
    Tag_Output -> printf "%s\n" msg

{-
## Logger computation
-}

log :: Member (Output Log) r => Tag -> String -> Sem r ()
log tag msg = output $ Log tag msg
