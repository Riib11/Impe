module Main.Excepting where

import qualified Language.Impe.Excepting
import Polysemy
import Polysemy.Error hiding (throw)
import qualified Polysemy.Error as Error
import Text.Printf
import Prelude hiding (exp)

{-
# Excepting
-}

{-
## Data
-}

data Exception
  = Interpretation Language.Impe.Excepting.Exception
  | Interaction Language.Impe.Excepting.Exception
  | Config String
  | Misc String

instance Show Exception where
  show = \case
    Interpretation excp -> show excp
    Interaction excp -> show excp
    Config msg -> printf "[exception] config\n%s" msg
    Misc msg -> printf "[exception] miscellaneous\n%s" msg

{-
## Excepting computation
-}

throw :: Member (Error Exception) r => Exception -> Sem r a
throw = Error.throw
