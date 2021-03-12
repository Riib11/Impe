module Main.Interacting.Parsing where

import Control.Monad
import Language.Impe.Excepting as Excepting
import qualified Language.Impe.Parsing as ImpeParsing
import Main.Interacting.Grammar
import Main.Interacting.Lexing
import Polysemy
import Polysemy.Error (Error)
import Text.ParserCombinators.Parsec
import Prelude hiding (interact, log)

{-
# Parsing
-}

type Parsed r a = Member (Error Exception) r => Sem r a

parseCommand :: String -> Parsed r Command
parseCommand = parsed command

parsed :: Parser a -> String -> Parsed r a
parsed parser source = case runParser parser () "stdin" source of
  Left prsErr -> throw . Excepting.Parsing $ prsErr
  Right x -> return x

{-
## Command
-}

command :: Parser Command
command =
  choice . map (try . flip constM eof) $
    [ Command_MetaCommand <$> metacommand,
      command_expression,
      Command_Instruction <$> ImpeParsing.instruction
    ]

command_expression :: Parser Command
command_expression = do
  void $ choice . map (try . symbol) $ [":e", ":eval"]
  Command_Expression <$> ImpeParsing.expression

{-
### MetaCommand
-}

metacommand :: Parser MetaCommand
metacommand =
  choice . map try $
    [ metacommand_context,
      metacommand_quit,
      metacommand_help
    ]

metacommand_context :: Parser MetaCommand
metacommand_context = do
  void $ choice . map (try . symbol) $ [":c", ":context"]
  return MetaCommand_Context

metacommand_quit :: Parser MetaCommand
metacommand_quit = do
  void $ choice . map (try . symbol) $ [":q", ":quit"]
  return MetaCommand_Quit

metacommand_help :: Parser MetaCommand
metacommand_help = do
  void $ choice . map (try . symbol) $ [":h", ":help"]
  return MetaCommand_Help

{-
# Utilities
-}

constM :: Monad m => m a -> m b -> m a
constM ma mb = ma >>= \x -> mb >> return x
