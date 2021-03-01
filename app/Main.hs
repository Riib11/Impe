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

main = return ()

-- type Main a = Sem '[Reader MainOptions, Error String, Embed IO] a

-- type InterpretationContext = (TypecheckingContext, ExecutionContext)

-- main :: IO ()
-- main = runM $ do
--   runError main' >>= \case
--     Left err -> embed . putStrLn $ err
--     Right () -> return ()

-- main' :: Sem '[Error String, Embed IO] ()
-- main' = do
--   opts <- embed $ execParser parseMainOptions
--   runReader opts do
--     asks mode >>= \case
--       Mode_Interpret -> mainInterpret
--       Mode_Interactive -> mainInteractive

-- mainInterpret :: Main ()
-- mainInterpret =
--   asks input_filename >>= \case
--     Nothing ->
--       throw "[options: error] must have an INPUT for interpret mode."
--     Just fn -> do
--       ctx <- interpretFile fn
--       case ctx ^. _2 . outputs of
--         [] -> return ()
--         outs -> embed . putStr . unlines $ outs

-- mainInteractive :: Main ()
-- mainInteractive = do
--   opts <- ask
--   ctx <-
--     asks input_filename >>= \case
--       Nothing -> interpretPrelude
--       Just fn -> interpretFile fn
--   embed $ repl opts ctx

-- interpretPrelude :: Main InterpretationContext
-- interpretPrelude = do
--   let prog = Program []
--   tcCtx <- mainTypecheckProgram prog
--   exCtx <- mainExecuteProgram prog
--   return (tcCtx, exCtx)

-- interpretFile :: String -> Main InterpretationContext
-- interpretFile fn = do
--   src <- mainReadInputFile fn
--   prog <- mainParseProgram fn src
--   tcCtx <- mainTypecheckProgram prog
--   exCtx <- mainExecuteProgram prog
--   return (tcCtx, exCtx)

-- mainReadInputFile :: String -> Main String
-- mainReadInputFile fn = do
--   mainDisplay Phase_Reading $ printf "[reading] %s" fn
--   embed $ readFile fn

-- mainParseProgram :: String -> String -> Main Program
-- mainParseProgram fn src = do
--   mainDisplay Phase_Parsing "[parsing]"
--   case runParser program () fn src of
--     Left err ->
--       throw . unlines $
--         ["[parsing error]", "", show err, ""]
--     Right prog -> do
--       mainDisplay Phase_Parsing . unlines $
--         ["[parsing success]", "", "program:", show prog, ""]
--       return prog

-- mainTypecheckProgram :: Program -> Main TypecheckingContext
-- mainTypecheckProgram prog = do
--   mainDisplay Phase_Typechecking "[typechecking]"
--   case runTypecheck emptyTypecheckingContext (processProgram prog) of
--     (logs, Left err) ->
--       throw . unlines $
--         ["[typecheck error]", "", "typechecking logs:", "  " ++ intercalate "\n  " logs, "", "typechecking error:", err]
--     (logs, Right (tcCtx, ())) -> do
--       mainDisplay Phase_Typechecking . unlines $
--         ["[typecheck success]", "", "typechecking logs:", "  " ++ intercalate "\n  " logs, "", show tcCtx, ""]
--       return tcCtx

-- mainExecuteProgram :: Program -> Main ExecutionContext
-- mainExecuteProgram prog = do
--   mainDisplay Phase_Executing "[executing]"
--   case runExecution emptyExecutionContext (executeProgram prog) of
--     (logs, Left err) ->
--       throw . unlines $
--         ["[execution error]", "", "execution logs:", "  " ++ intercalate "\n  " logs, "", "execution error:", show err]
--     (logs, Right (exCtx, ())) -> do
--       mainDisplay Phase_Executing . unlines $
--         ["[execution success]", "", "execution logs:", "  " ++ intercalate "\n  " logs, "", show exCtx]
--       return exCtx

-- {-
-- ## Utilities
-- -}

-- mainDisplay :: Phase -> String -> Main ()
-- mainDisplay phase msg = do
--   phases <- asks verbosity
--   when (phase `elem` phases) $
--     embed (putStrLn msg)

-- replDisplay :: Phase -> String -> REPL ()
-- replDisplay phase msg = do
--   phases <- asks verbosity
--   when (phase `elem` phases) $
--     embed (putStrLn msg)
