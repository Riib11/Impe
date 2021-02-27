{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Lens
import Control.Monad
import Data.Version (showVersion)
import Development.GitRev (gitHash)
import Language.Impe.Executing
import Language.Impe.Grammar
import Language.Impe.Parsing
import Language.Impe.Typechecking
import MainOptions
import Options.Applicative
import Paths_impe (version)
import Polysemy
import Polysemy.Error as Error
import Polysemy.Output as Output
import Polysemy.Reader as Reader
import Polysemy.State as State
import Text.ParserCombinators.Parsec (runParser)
import Text.Printf

type Main a = Sem '[Reader MainOptions, Error MainError, Embed IO] a

type MainError = String

main :: IO ()
main = runM $ do
  runError main' >>= \case
    Left err -> embed . putStrLn $ err
    Right () -> return ()

main' :: Sem '[Error MainError, Embed IO] ()
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
      exctx <- interpretFile fn
      case exctx ^. outputs of
        [] ->
          return ()
        outs ->
          embed . putStr . unlines $ outs

mainInteractive :: Main ()
mainInteractive =
  asks input_filename >>= \case
    Nothing -> do
      repl emptyExecutionContext
    Just fn -> do
      exctx <- interpretFile fn
      repl exctx

interpretFile :: String -> Main ExecutionContext
interpretFile fn = do
  src <- readInputFile fn
  prog <- mainParseProgram fn src
  _ <- mainTypecheckProgram prog
  exctx <- mainExecuteProgram prog
  return exctx

readInputFile :: String -> Main String
readInputFile fn = do
  display Phase_Reading $ printf "[reading] %s" fn
  embed $ readFile fn

mainParseProgram :: String -> String -> Main Program
mainParseProgram fn src = do
  display Phase_Parsing "[parsing]"
  case runParser program () fn src of
    Left err ->
      throw . unlines $
        ["[parsing error]", "", show err, ""]
    Right prog -> do
      display Phase_Parsing . unlines $
        ["[parsing success]", "", "program:", show prog, ""]
      return prog

mainTypecheckProgram :: Program -> Main TypecheckingContext
mainTypecheckProgram prog = do
  display Phase_Typechecking "[typechecking]"
  case execTypecheck (processProgram prog) of
    (logs, Left err) ->
      throw . unlines $
        ["[typecheck error]", "", "typechecking logs:", show logs, "", "typechecking error:", show err]
    (logs, Right tcctx) -> do
      display Phase_Typechecking . unlines $
        ["[typecheck success]", "", "typechecking logs:", show logs, "", "typechecking context:", show tcctx, ""]
      return tcctx

mainExecuteProgram :: Program -> Main ExecutionContext
mainExecuteProgram prog = do
  display Phase_Executing "[executing]"
  case execExecution (executeProgram prog) of
    (logs, Left err) ->
      throw . unlines $
        ["[execution error]", "", "execution logs:", show logs, "", "execution error:", show err]
    (logs, Right exctx) -> do
      display Phase_Executing . unlines $
        ["[execution success]", "", "execution logs:", show logs, "", "execution context:", show exctx]
      return exctx

-- TODO
repl :: ExecutionContext -> Main ()
repl exctx = return ()

{-
## Utilities
-}

display :: Phase -> String -> Main ()
display phase msg = do
  phases <- asks verbosity
  when (phase `elem` phases) $
    embed (putStrLn msg)
