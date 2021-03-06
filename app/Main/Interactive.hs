module Main.Interactive where

import Control.Lens
import Control.Monad
import Data.List (intercalate)
import Data.Version (showVersion)
import Development.GitRev (gitHash)
import Language.Impe.Executing as Executing
import Language.Impe.Grammar as Grammar
import Language.Impe.Interpreting as Interpreting
import Language.Impe.Parsing as Parsing
import Language.Impe.Typechecking as Typechecking
import Main.Interactive.Grammar
import Main.Interactive.Parsing
import Main.MainOptions
import Paths_impe (version)
import Polysemy
import Polysemy.Error as Error
import Polysemy.Fail as Fail
import Polysemy.Output as Output
import Polysemy.Reader as Reader
import Polysemy.State as State
import System.IO as IO
import Text.ParserCombinators.Parsec (runParser)
import Text.Printf
import Prelude hiding (log)

{-
# Interactive
-}

{-
# Data
-}

data InteractiveContext = InteractiveContext
  { _typecheckingContext :: Typechecking.Context,
    _executionContext :: Executing.Context
  }

makeLenses ''InteractiveContext

{-
# Interface
-}

startInteractive :: Sem '[Reader MainOptions, Error MainError, Embed IO] ()
startInteractive = do
  embed $ putStrLn "[impe - interactive mode]"
  prgm <-
    asks input_filename >>= \case
      Nothing ->
        return $ Program []
      Just fn -> do
        --
        -- read
        --
        log Phase_Reading $ printf "reading input file: %s\n" fn
        src <- embed $ readFile fn
        log Phase_Reading $ printf "read source:\n\n%s\n" src
        --
        -- parse
        --
        log Phase_Parsing $ "parsing source\n"
        prgm <- case runParser program () fn src of
          Left prsErr -> throw . MainError $ printf "[parsing error]\n%s\n" (show prsErr)
          Right prgm -> return prgm
        log Phase_Parsing $ printf "parsed program:\n\n%s\n" (show prgm)
        return prgm
  --
  -- typecheck
  --
  tchCtx <-
    (raise_ . runOutputList . runState Typechecking.emptyContext . runError)
      (typecheckProgram prgm)
      >>= \case
        (logs, (tchCtx, Left err)) ->
          throw . MainError . concat $
            [ printf "[typechecking] logs\n\n%s\n" (unlines logs),
              printf "[typechecking] context\n\n%s" (show tchCtx),
              printf "[typechecking] error\n\n%s\n" err
            ]
        (logs, (tchCtx, Right ())) -> do
          log Phase_Typechecking $ printf "[typechecking logs\n\n%s" (unlines logs)
          log Phase_Typechecking $ printf "[typechecking] context\n\n%s" (show tchCtx)
          return tchCtx
  --
  -- execute
  --
  log Phase_Executing $ "executing program\n"
  exeCtx <-
    (raise_ . runOutputList . runState Executing.emptyContext . runError)
      (executeProgram prgm)
      >>= \case
        (logs, (exeCtx, Left err)) ->
          throw . MainError . concat $
            [ printf "[execution logs]\n%s\n" (unlines logs),
              printf "[execution context]\n%s" (show exeCtx),
              printf "[execution error]\n%s\n" err
            ]
        (logs, (exeCtx, Right ())) -> do
          log Phase_Executing $ printf "[execution] logs\n\n%s" (unlines logs)
          log Phase_Executing $ printf "[execution] context\n\n%s" (show exeCtx)
          --
          -- output
          --
          case exeCtx ^. outputs . to reverse of
            [] -> return ()
            os -> embed . putStr . unlines $ os
          --
          -- reset IO
          --
          return $
            exeCtx & outputs .~ []
  --
  -- REPL loop
  --
  void $
    runState
      ( InteractiveContext
          { _typecheckingContext = tchCtx,
            _executionContext = exeCtx
          }
      )
      repl

repl :: Sem '[State InteractiveContext, Reader MainOptions, Error MainError, Embed IO] ()
repl =
  runError replLoop >>= \case
    Left (InteractiveError err) -> do
      embed $ putStr err
      repl
    Right () ->
      repl

{-
## Interactive computations
-}

type Interactive a =
  Sem '[Error InteractiveError, State InteractiveContext, Reader MainOptions, Error MainError, Embed IO] a

replLoop :: Interactive ()
replLoop = do
  --
  -- read
  --
  embed $ putStr "> " >> hFlush stdout
  src <- embed getLine
  --
  -- parse
  --
  log Phase_Parsing $ "parsing input\n"
  cmd <- case runParser command () "stdin" src of
    Left prsErr -> throw . InteractiveError $ printf "[parsing error]\n%s\n" (show prsErr)
    Right cmd -> return cmd
  log Phase_Parsing $ printf "parsed command: %s\n" (show cmd)
  --
  -- run command
  --
  replCommand cmd
  --
  -- loop
  --
  replLoop

replCommand :: Command -> Interactive ()
replCommand cmd = case cmd of
  Command_Expression e -> replInstruction (Return e)
  Command_Instruction inst -> replInstruction inst
  Command_MetaCommand metcmd -> replMetaCommand metcmd

replMetaCommand :: MetaCommand -> Interactive ()
replMetaCommand metcmd = case metcmd of
  MetaCommand_Context -> do
    embed . putStrLn $ "[impe - interactive mode] context"
    embed . putStr . show =<< gets (^. typecheckingContext)
    embed . putStr . show =<< gets (^. executionContext)
  MetaCommand_Quit -> do
    embed . putStrLn $ "[impe - interactive mode] exit"
    throw $ MainExit
  MetaCommand_Help ->
    embed . putStr . unlines $
      [ "[impe - interactive mode] help",
        "commands:",
        "  :[c|context] -- current context",
        "  :[q|quit]    -- quit interactive REPL",
        "  :[h|help]    -- help"
      ]

replExpression :: Expression -> Interactive ()
replExpression e = do
  --
  -- typecheck
  --
  log Phase_Typechecking $ "typechecking expression\n"
  tchCtx <- gets (^. typecheckingContext)
  (tchCtx', t) <-
    (raise_ . runOutputList . runState tchCtx . runError)
      (synthesizeExpression e)
      >>= \case
        (logs, (tchCtx, Left err)) ->
          throw . InteractiveError . concat $
            [ printf "[typechecking] logs\n\n%s\n" (unlines logs),
              printf "[typechecking] context\n\n%s" (show tchCtx),
              printf "[typechecking] error\n\n%s\n" err
            ]
        (logs, (tchCtx, Right t)) -> do
          log Phase_Typechecking $ printf "[typechecking] logs\n\n%s" (unlines logs)
          log Phase_Typechecking $ printf "[typechecking] context\n\n%s" (show tchCtx)
          return (tchCtx, t)
  --
  -- execute
  --
  log Phase_Executing $ "executing expression\n"
  exeCtx <- gets (^. executionContext)
  (exeCtx', v) <-
    (raise_ . runOutputList . runState exeCtx . runError)
      (evaluateExpression e)
      >>= \case
        (logs, (exeCtx, Left err)) ->
          throw . InteractiveError . concat $
            [ printf "[execution logs]\n%s\n" (unlines logs),
              printf "[execution context]\n%s" (show exeCtx),
              printf "[execution error]\n%s\n" err
            ]
        (logs, (exeCtx, Right mb_v)) -> do
          log Phase_Executing $ printf "[execution] logs\n\n%s" (unlines logs)
          log Phase_Executing $ printf "[execution] context\n\n%s" (show exeCtx)
          return (exeCtx, mb_v)
  --
  -- output
  --
  case exeCtx' ^. outputs of
    [] -> return ()
    os -> embed . putStr . unlines $ os
  --
  -- value
  --
  embed $ printf "%s : %s" (show v) (show t)
  --
  -- update context
  --
  modify $ typecheckingContext .~ tchCtx'
  modify $ executionContext .~ exeCtx'
  --
  -- reset IO
  --
  modify $ executionContext . outputs .~ []

replInstruction :: Instruction -> Interactive ()
replInstruction inst = do
  --
  -- typecheck
  --
  log Phase_Typechecking $ "typechecking instruction\n"
  tchCtx <- gets (^. typecheckingContext)
  tchCtx' <-
    (raise_ . runOutputList . runState tchCtx . runError)
      (synthesizeInstruction inst)
      >>= \case
        (logs, (tchCtx, Left err)) ->
          throw . InteractiveError . concat $
            [ printf "[typechecking] logs\n\n%s\n" (unlines logs),
              printf "[typechecking] context\n\n%s" (show tchCtx),
              printf "[typechecking] error\n\n%s\n" err
            ]
        (logs, (tchCtx, Right t)) -> do
          log Phase_Typechecking $ printf "[typechecking] logs\n\n%s" (unlines logs)
          log Phase_Typechecking $ printf "[typechecking] context\n\n%s" (show tchCtx)
          return tchCtx
  --
  -- execute
  --
  log Phase_Executing $ "executing instruction\n"
  exeCtx <- gets (^. executionContext)
  (exeCtx', mb_v) <-
    (raise_ . runOutputList . runState exeCtx . runError)
      (executeInstruction inst)
      >>= \case
        (logs, (exeCtx, Left err)) ->
          throw . InteractiveError . concat $
            [ printf "[execution logs]\n%s\n" (unlines logs),
              printf "[execution context]\n%s" (show exeCtx),
              printf "[execution error]\n%s\n" err
            ]
        (logs, (exeCtx, Right mb_v)) -> do
          log Phase_Executing $ printf "[execution] logs\n\n%s" (unlines logs)
          log Phase_Executing $ printf "[execution] context\n\n%s" (show exeCtx)
          return (exeCtx, mb_v)
  --
  -- output
  --
  case exeCtx' ^. outputs of
    [] -> return ()
    os -> embed . putStr . unlines $ os
  --
  -- value
  --
  case mb_v of
    Just v -> embed . putStrLn $ show v
    Nothing -> return ()
  --
  -- update context
  --
  modify $ typecheckingContext .~ tchCtx'
  modify $ executionContext .~ exeCtx'
  --
  -- reset IO
  --
  modify $ executionContext . outputs .~ []

{-
### Logging
-}

log ::
  (Member (Reader MainOptions) r, Member (Embed IO) r) =>
  Phase ->
  String ->
  Sem r ()
log phs msg =
  elem phs <$> asks verbosity >>= \case
    True -> embed . putStr $ printf "[%s] %s" (show phs) msg
    False -> return ()
