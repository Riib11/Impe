module Main.Interacting.Grammar where

import Language.Impe.Grammar

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
