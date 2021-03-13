module Main.Excepting
  ( Exception (..),
    Config (..),
    throw,
  )
where

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
  | Config Config
  | Misc String

instance Show Exception where
  show = \case
    Interpretation excp -> show excp
    Interaction excp -> show excp
    Config cfgErr -> printf "[exception] config\n%s" (show cfgErr)
    Misc msg -> printf "[exception] miscellaneous\n%s" msg

data Config
  = MissingSource

instance Show Config where
  show = \case
    MissingSource -> "requires a source file"

{-
## Excepting computation
-}

throw :: Member (Error Exception) r => Exception -> Sem r a
throw = Error.throw
