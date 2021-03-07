module Main.Config.Grammar where

import qualified Language.Impe.Logging as Logging

data Config = Config
  { mode :: Mode,
    verbosity :: Verbosity,
    source_filename :: Maybe String
  }

newtype Verbosity = Verbosity [Logging.Tag]

data Mode
  = Mode_Interpret
  | Mode_Interact
