{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Control.Lens
import Control.Monad
import Data.List (intercalate)
import Data.Version (showVersion)
import Development.GitRev (gitHash)
import Language.Impe.Executing as Executing
import Language.Impe.Grammar as Grammar
import Language.Impe.Parsing as Parsing
import Language.Impe.Typechecking as Typechecking
import MainOptions
import Options.Applicative
import Paths_impe (version)
import Polysemy
import Polysemy.Error as Error
import Polysemy.Output as Output
import Polysemy.Reader as Reader
import Polysemy.State as State
import System.IO as IO
import Text.ParserCombinators.Parsec (runParser)
import Text.Printf

type Main a = Sem '[Reader MainOptions, Error String, Embed IO] a

type InterpretationContext = (TypecheckingContext, ExecutionContext)

main :: IO ()
main = runM $ do
  runError main' >>= \case
    Left err -> embed . putStrLn $ err
    Right () -> return ()

main' :: Sem '[Error String, Embed IO] ()
main' = do
  opts <- embed $ execParser parseMainOptions
  runReader opts do
    asks mode >>= \case
      Mode_Interpret -> mainInterpret
      Mode_Interactive -> mainInteractive

mainInterpret :: Main ()
mainInterpret =
  asks input_filename >>= \case
    Nothing ->
      throw "[options: error] must have an INPUT for interpret mode."
    Just fn -> do
      ctx <- interpretFile fn
      case ctx ^. _2 . outputs of
        [] -> return ()
        outs -> embed . putStr . unlines $ outs

mainInteractive :: Main ()
mainInteractive = do
  opts <- ask
  ctx <-
    asks input_filename >>= \case
      Nothing -> interpretPrelude
      Just fn -> interpretFile fn
  embed $ repl opts ctx

interpretPrelude :: Main InterpretationContext
interpretPrelude = do
  let prog = Program []
  tcctx <- mainTypecheckProgram prog
  exctx <- mainExecuteProgram prog
  return (tcctx, exctx)

interpretFile :: String -> Main InterpretationContext
interpretFile fn = do
  src <- mainReadInputFile fn
  prog <- mainParseProgram fn src
  tcctx <- mainTypecheckProgram prog
  exctx <- mainExecuteProgram prog
  return (tcctx, exctx)

mainReadInputFile :: String -> Main String
mainReadInputFile fn = do
  mainDisplay Phase_Reading $ printf "[reading] %s" fn
  embed $ readFile fn

mainParseProgram :: String -> String -> Main Program
mainParseProgram fn src = do
  mainDisplay Phase_Parsing "[parsing]"
  case runParser program () fn src of
    Left err ->
      throw . unlines $
        ["[parsing error]", "", show err, ""]
    Right prog -> do
      mainDisplay Phase_Parsing . unlines $
        ["[parsing success]", "", "program:", show prog, ""]
      return prog

mainTypecheckProgram :: Program -> Main TypecheckingContext
mainTypecheckProgram prog = do
  mainDisplay Phase_Typechecking "[typechecking]"
  case runTypecheck emptyTypecheckingContext (processProgram prog) of
    (logs, Left err) ->
      throw . unlines $
        ["[typecheck error]", "", "typechecking logs:", "  " ++ intercalate "\n  " logs, "", "typechecking error:", err]
    (logs, Right (tcctx, ())) -> do
      mainDisplay Phase_Typechecking . unlines $
        ["[typecheck success]", "", "typechecking logs:", "  " ++ intercalate "\n  " logs, "", show tcctx, ""]
      return tcctx

mainExecuteProgram :: Program -> Main ExecutionContext
mainExecuteProgram prog = do
  mainDisplay Phase_Executing "[executing]"
  case runExecution emptyExecutionContext (executeProgram prog) of
    (logs, Left err) ->
      throw . unlines $
        ["[execution error]", "", "execution logs:", "  " ++ intercalate "\n  " logs, "", "execution error:", show err]
    (logs, Right (exctx, ())) -> do
      mainDisplay Phase_Executing . unlines $
        ["[execution success]", "", "execution logs:", "  " ++ intercalate "\n  " logs, "", show exctx]
      return exctx

{-
## Interactive REPL
-}

type REPL a = Sem '[Error String, Reader MainOptions, Error (), Embed IO] a

repl :: MainOptions -> InterpretationContext -> IO ()
repl opts ctx = do
  putStrLn "[impe: interactive]"
  void . runM . runError . runReader opts $ replLoop ctx

replLoop :: InterpretationContext -> Sem '[Reader MainOptions, Error (), Embed IO] a
replLoop ctx = do
  -- execute
  (ctx', mb_v_t) <-
    runError (replLoopBody ctx) >>= \case
      Left err -> do
        embed . putStrLn $ err
        return (ctx, Nothing)
      Right (ctx', mb_v_t) ->
        return (ctx', mb_v_t)
  -- output
  case ctx' ^. _2 . outputs of
    [] -> return ()
    outs -> embed . putStr . unlines $ outs
  -- value and type
  case mb_v_t of
    Just (v, t) -> embed . putStrLn $ printf "%s: %s" (show v) (show t)
    Nothing -> return ()
  -- reset outputs and logs
  let ctx'' = ctx' & _2 . outputs .~ []
  -- recurse
  replLoop ctx''

replLoopBody :: InterpretationContext -> REPL (InterpretationContext, Maybe (Value, Type))
replLoopBody ctx = do
  src <- replReadInput
  replParseInput src >>= \case
    InstructionREPL inst -> do
      (tcctx, t) <- replTypecheckInstruction ctx inst
      (exctx', mb_v) <- replExecuteInstruction ctx inst
      return ((tcctx, exctx'), (,t) <$> mb_v)
    ExpressionREPL e -> do
      -- TODO: does this actually work right???
      let inst = Return e
      (tcctx, t) <- replTypecheckInstruction ctx inst
      (exctx', mb_v) <- replExecuteInstruction ctx inst
      return ((tcctx, exctx'), (,t) <$> mb_v)
    CommandREPL cmd ->
      case cmd of
        CommandREPL_Quit -> throw () -- escape REPL
        CommandREPL_GetContext -> do
          embed
            do
              putStrLn . show $ ctx ^. _1
              putStrLn . show $ ctx ^. _2
          return (ctx, Nothing)
        CommandREPL_GetType inst -> do
          (tcctx, t) <- replTypecheckInstruction ctx inst
          embed . putStrLn . show $ t
          return (ctx, Nothing)

replReadInput :: REPL String
replReadInput = embed do
  putStr "> "
  hFlush stdout
  getLine

replParseInput :: String -> REPL InputREPL
replParseInput src = do
  replDisplay Phase_Parsing "[parsing]"
  case runParser inputREPL () "stdin" src of
    Left err ->
      throw . unlines $
        ["[parsing error]", "", show err, ""]
    Right inpt -> do
      replDisplay Phase_Parsing . unlines $
        ["[parsing success]", "", "input:", show inpt, ""]
      return inpt

replTypecheckInstruction :: InterpretationContext -> Instruction -> REPL (TypecheckingContext, Type)
replTypecheckInstruction ctx inst = do
  replDisplay Phase_Typechecking "[typechecking]"
  case runTypecheck (ctx ^. _1) (synthesizeInstruction inst) of
    (logs, Left err) ->
      throw . unlines $
        ["[typecheck error]", "", "typechecking logs:", "  " ++ intercalate "\n  " logs, "", "typechecking error: " ++ err]
    (logs, Right (tcctx, t)) -> do
      replDisplay Phase_Typechecking . unlines $
        ["[typecheck success]", "", "typechecking logs:", "  " ++ intercalate "\n  " logs, "", show tcctx, ""]
      return (tcctx, t)

replExecuteInstruction :: InterpretationContext -> Instruction -> REPL (ExecutionContext, Maybe Value)
replExecuteInstruction ctx inst = do
  replDisplay Phase_Executing "[executing]"
  case runExecution (ctx ^. _2) (executeInstruction inst) of
    (logs, Left err) ->
      throw . unlines $
        ["[execution error]", "", "execution logs:", "  " ++ intercalate "\n  " logs, "", "execution error:", show err]
    (logs, Right (exctx, mb_v)) -> do
      replDisplay Phase_Executing . unlines $
        ["[execution success]", "", "execution logs:", "  " ++ intercalate "\n  " logs, "", show exctx]
      return (exctx, mb_v)

{-
## Utilities
-}

mainDisplay :: Phase -> String -> Main ()
mainDisplay phase msg = do
  phases <- asks verbosity
  when (phase `elem` phases) $
    embed (putStrLn msg)

replDisplay :: Phase -> String -> REPL ()
replDisplay phase msg = do
  phases <- asks verbosity
  when (phase `elem` phases) $
    embed (putStrLn msg)
