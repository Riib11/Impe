module Main.Interacting where

import Control.Monad
import Data.List (intercalate)
import qualified Language.Impe.Excepting as ImpeExcepting
import qualified Language.Impe.Executing as Executing
import Language.Impe.Interpreting
import Language.Impe.Logging
import qualified Language.Impe.Typechecking as Typechecking
import qualified Main.Excepting as MainExcepting
import Main.Interacting.Grammar
import Main.Interacting.Parsing
import Polysemy
import Polysemy.Error (Error, runError)
import Polysemy.Output (Output)
import Polysemy.State
import System.IO hiding (interact)
import Text.Printf
import Prelude hiding (interact, log)

interact ::
  ( Member (Output Log) r,
    Member (State Typechecking.Context) r,
    Member (State Executing.Context) r,
    Member (Error MainExcepting.Exception) r,
    Member (Embed IO) r
  ) =>
  Sem r ()
interact = do
  continue <-
    (runError :: Sem (Error ImpeExcepting.Exception : r) Bool -> Sem r (Either ImpeExcepting.Exception Bool)) interactStep >>= \case
      Left err -> do
        log Tag_Error $ printf "%s" (show err)
        return True
      Right b -> return b
  if continue
    then interact
    else log Tag_Output "[impe - interact] quit"

interactStep ::
  ( Member (Output Log) r,
    Member (State Typechecking.Context) r,
    Member (State Executing.Context) r,
    Member (Error ImpeExcepting.Exception) r,
    Member (Error MainExcepting.Exception) r,
    Member (Embed IO) r
  ) =>
  Sem r Bool
interactStep = do
  -- prompt
  embed do
    putStr "> "
    hFlush stdout
  src <- embed getLine
  -- parse command
  log Tag_Debug $ "parsing command"
  cmd <- parseCommand src
  log Tag_Debug $ printf "parsed command: %s" (show cmd)
  -- handle command
  b <-
    parseCommand src >>= \case
      Command_Instruction inst -> do
        -- interpret input
        (mb_v, t) <- interpretInstructionParsed inst
        -- handle outputs
        Executing.logOutputs
        Executing.resetOutputs
        -- result
        case mb_v of
          Just v -> log Tag_Output $ printf "returns %s :: %s" (show v) (show t)
          Nothing -> return ()
        -- continue
        return True
      Command_Expression expr -> do
        -- interpret input
        (v, t) <- interpretExpressionParsed expr
        -- handle outputs
        Executing.logOutputs
        Executing.resetOutputs
        -- result
        log Tag_Output $ printf "%s :: %s" (show v) (show t)
        -- continue
        return True
      Command_MetaCommand mtacmd -> interpretMetaCommand mtacmd
  log Tag_Output $ printf "\n"
  return b

interpretMetaCommand ::
  ( Member (Output Log) r,
    Member (State Typechecking.Context) r,
    Member (State Executing.Context) r,
    Member (Error ImpeExcepting.Exception) r,
    Member (Error MainExcepting.Exception) r,
    Member (Embed IO) r
  ) =>
  MetaCommand ->
  Sem r Bool
interpretMetaCommand = \case
  MetaCommand_Context -> do
    -- log contexts
    tchCtx <- get :: Member (State Typechecking.Context) r => Sem r Typechecking.Context
    log Tag_Output $ printf "typechecking context:\n\n%s\n" (show tchCtx)
    exeCtx <- get :: Member (State Executing.Context) r => Sem r Executing.Context
    log Tag_Output $ printf "executing context:\n\n%s\n" (show exeCtx)
    -- continue
    return True
  MetaCommand_Help -> do
    log Tag_Output . intercalate "\n" $
      [ "[impe - interact] help",
        "TODO"
      ]
    -- continue
    return True
  MetaCommand_Quit ->
    -- quit
    return False
