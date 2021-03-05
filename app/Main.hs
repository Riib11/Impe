module Main where

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
import MainOptions
import Options.Applicative
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

main :: IO ()
main =
  (runM . runFail) start >>= \case
    Left err -> putStr err
    Right () -> return ()

start :: Sem '[Fail, Embed IO] ()
start = do
  -- options
  opts <- embed $ execParser parseMainOptions
  -- mode
  runReader opts do
    asks mode >>= \case
      Mode_Interpret -> startInterpret
      Mode_Interactive -> startInteractive

-- startInterpret :: Sem '[Reader MainOptions, Fail, Embed IO] ()
-- startInterpret = do
--   -- filename
--   fn <-
--     asks input_filename >>= \case
--       Nothing ->
--         fail $ printf "[options error] options must provide an INPUT in order to use interpret mode"
--       Just fn -> return fn
--   -- read
--   log Phase_Reading $ printf "reading input file: %s" fn
--   src <- embed $ readFile fn
--   log Phase_Reading $ printf "read source:\n\n%s\n" src
--   -- parse
--   log Phase_Parsing $ printf "parsing source"
--   prgm <- case runParser program () fn src of
--     Left prsErr -> fail $ printf "[parsing error] %s" (show prsErr)
--     Right prgm -> return prgm
--   log Phase_Parsing $ printf "parsed program:\n\n%s\n" (show prgm)
--   -- interpret
--   log Phase_Interpreting $ printf "interpreting program"
--   itpCtx <-
--     (raise_ . runOutputList . runError . execState Interpreting.emptyContext)
--       (interpretProgram prgm)
--       >>= \case
--         (logs, Left err) -> fail err
--         (logs, Right itpCtx) -> return itpCtx
--   log Phase_Interpreting $ printf "interpreted context:\n\n%s\n" (show itpCtx)
--   -- output
--   embed . putStr . unlines $ itpCtx ^. executionContext . outputs
--   -- done
--   return ()

startInterpret :: Sem '[Reader MainOptions, Fail, Embed IO] ()
startInterpret = do
  --
  -- filename
  --
  fn <-
    asks input_filename >>= \case
      Nothing ->
        fail $ printf "[options error]\noptions must provide an INPUT in order to use interpret mode\n"
      Just fn -> return fn
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
    Left prsErr -> fail $ printf "[parsing error]\n%s" (show prsErr)
    Right prgm -> return prgm
  log Phase_Parsing $ printf "parsed program:\n\n%s\n" (show prgm)
  --
  -- typecheck
  --
  log Phase_Typechecking $ "typechecking program\n"
  tchCtx <-
    (raise_ . runOutputList . runState Typechecking.emptyContext . runError)
      (typecheckProgram prgm)
      >>= \case
        (logs, (tchCtx, Left err)) ->
          fail . concat $
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
          fail . concat $
            [ printf "[execution logs]\n%s\n" (unlines logs),
              printf "[execution context]\n%s" (show exeCtx),
              printf "[execution error]\n%s\n" err
            ]
        (logs, (exeCtx, Right ())) -> do
          log Phase_Executing $ printf "[execution] logs\n\n%s" (unlines logs)
          log Phase_Executing $ printf "[execution] context\n\n%s" (show exeCtx)
          return exeCtx
  --
  -- output
  --
  embed . putStr . unlines $ exeCtx ^. outputs
  --
  -- done
  --
  return ()

startInteractive :: Sem '[Reader MainOptions, Fail, Embed IO] ()
startInteractive = return () -- TODO

log :: Phase -> String -> Sem '[Reader MainOptions, Fail, Embed IO] ()
log phs msg =
  elem phs <$> asks verbosity >>= \case
    True -> embed . putStr $ printf "[%s] %s" (show phs) msg
    False -> return ()
