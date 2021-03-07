module Language.Impe.Excepting where

import Polysemy
import Polysemy.Error hiding (throw)
import qualified Polysemy.Error as Error
import Text.Parsec (ParseError)
import Text.Printf

{-
# Excepting
-}

{-
## Data
-}

data Exception
  = Exception_Parsing ParseError
  | Exception_Misc String

instance Show Exception where
  show = \case
    Exception_Misc msg -> printf "[exception] miscellaneous\n%s" msg
    Exception_Parsing prsErr -> show prsErr

{-
## Excepting computation
-}

throw :: Member (Error Exception) r => Exception -> Sem r a
throw = Error.throw
