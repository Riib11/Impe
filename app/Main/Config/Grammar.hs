module Main.Config.Grammar where

import Data.Map as Map
import Language.Impe.Logging

data Config = Config
  { mode :: Mode,
    verbosity :: Verbosity,
    source_filename :: Maybe String
  }

newtype Verbosity = Verbosity [Tag]

verbosities :: Map String Verbosity
verbosities =
  Map.map Verbosity . fromList $
    [ ("debug", [Tag_Error, Tag_Output, Tag_Warning, Tag_Debug]),
      ("normal", [Tag_Error, Tag_Output, Tag_Warning]),
      ("quiet", [Tag_Error, Tag_Output]),
      ("silent", [Tag_Error]),
      ("arrogant", [])
    ]

data Mode
  = Mode_Interpret
  | Mode_Interact
