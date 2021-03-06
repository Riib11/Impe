module Main.Interactive.Grammar where

import Language.Impe.Grammar
import Text.ParserCombinators.Parsec

data Command
  = Command_Instruction Instruction
  | Command_Expression Expression
  | Command_MetaCommand MetaCommand
  deriving (Show)

data MetaCommand
  = MetaCommand_Context
  | MetaCommand_Quit
  | MetaCommand_Help
  deriving (Show)

{-
## Error
-}

data MainError
  = MainError String
  | MainExit

data InteractiveError
  = InteractiveError String
