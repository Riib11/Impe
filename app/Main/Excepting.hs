module Main.Excepting where

import qualified Language.Impe.Excepting
import Polysemy
import Polysemy.Error hiding (throw)
import qualified Polysemy.Error as Error
import Text.Printf

{-
# Excepting
-}

{-
## Data
-}

data Exception
  = Exception_Interpretation Language.Impe.Excepting.Exception
  | Exception_Interaction Language.Impe.Excepting.Exception
  | Exception_Config String
  | Exception_Misc String

instance Show Exception where
  show = \case
    Exception_Interpretation exp -> show exp
    Exception_Interaction exp -> show exp
    Exception_Config msg -> printf "[exception] config\n%s" msg
    Exception_Misc msg -> printf "[exception] miscellaneous\n%s" msg

{-
## Excepting computation
-}

throw :: Member (Error Exception) r => Exception -> Sem r a
throw = Error.throw
