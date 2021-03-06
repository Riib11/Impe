module Main.Interactive.Parsing where

import Language.Impe.Parsing
import Main.Interactive.Grammar
import Main.Interactive.Lexing
import Text.ParserCombinators.Parsec

-- command

command :: Parser Command
command = do
  cmd <-
    choice . map try $
      [ command_metacommand,
        command_expression,
        command_instruction
      ]
  eof
  return cmd

command_expression :: Parser Command
command_expression = do
  choice . map (try . symbol) $ [":e", ":eval"]
  Command_Expression <$> expression

command_instruction :: Parser Command
command_instruction =
  Command_Instruction <$> instruction

-- metacommand

command_metacommand :: Parser Command
command_metacommand = Command_MetaCommand <$> metacommand

metacommand :: Parser MetaCommand
metacommand =
  choice . map try $
    [ metacommand_context,
      metacommand_quit,
      metacommand_help
    ]

metacommand_context :: Parser MetaCommand
metacommand_context = do
  choice . map (try . symbol) $ [":c", ":context"]
  return MetaCommand_Context

metacommand_quit :: Parser MetaCommand
metacommand_quit = do
  choice . map (try . symbol) $ [":q", ":quit"]
  return MetaCommand_Quit

metacommand_help :: Parser MetaCommand
metacommand_help = do
  choice . map (try . symbol) $ [":h", ":help"]
  return MetaCommand_Help
