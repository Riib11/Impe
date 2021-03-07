module Main where

import Control.Lens
import Control.Monad
import Data.List (intercalate)
import Data.Version (showVersion)
import Development.GitRev (gitHash)
import qualified Language.Impe.Executing as Executing
import Language.Impe.Interpreting
import Language.Impe.Logging
import qualified Language.Impe.Typechecking as Typechecking
import Main.Config
import Main.Config.Grammar
import Main.Excepting as Excepting
import Main.Interacting as Interacting
import Options.Applicative
import Paths_impe (version)
import Polysemy hiding (interpret)
import Polysemy.Error (Error, runError)
import Polysemy.Output
import Polysemy.Reader
import Polysemy.State
import System.IO as IO hiding (interact)
import Text.ParserCombinators.Parsec (runParser)
import Text.Printf
import Prelude hiding (interact, log)

{-
# Main
-}

main :: IO ()
main =
  (runM . runError) start >>= \case
    Left err -> print err
    Right () -> return ()

start ::
  Sem
    '[ Error Exception,
       Embed IO
     ]
    ()
start = do
  cfg <- parseConfig
  case cfg & mode of
    Mode_Interpret ->
      runReader cfg startInterpret
    Mode_Interact ->
      runReader cfg startInteract

startInterpret ::
  Sem
    '[ Reader Config,
       Error Exception,
       Embed IO
     ]
    ()
startInterpret = do
  -- get filename
  fn <-
    asks source_filename >>= \case
      Just fn -> return fn
      Nothing -> throw . Exception_Config $ "interpretation mode requires an input source file"
  -- read source
  src <- embed $ readFile fn
  ( runError
      . evalState Executing.emptyContext
      . evalState Typechecking.emptyContext
      . runOutputSem handleOutputLog
    )
    ( do
        -- interpret source
        interpretProgram fn src
        -- log outputs
        Executing.logOutputs
    )
    >>= \case
      Left exp -> throw . Exception_Interpretation $ exp
      Right () -> return ()

startInteract ::
  Sem
    '[ Reader Config,
       Error Exception,
       Embed IO
     ]
    ()
startInteract = do
  -- get filename and source
  (fn, src) <-
    asks source_filename >>= \case
      -- get filename
      Just fn -> do
        src <- embed $ readFile fn
        return (fn, src)
      -- default empty source file
      Nothing ->
        return ("default", "")

  ( runError
      . evalState Executing.emptyContext
      . evalState Typechecking.emptyContext
      . runOutputSem handleOutputLog
    )
    ( do
        -- interpret source
        interpretProgram fn src
        log Tag_Output "[impe - interact] start"
        -- interact with context
        interact
    )
    >>= \case
      Left exp -> throw . Exception_Interaction $ exp
      Right () -> return ()

handleOutputLog ::
  ( Member (Embed IO) r,
    Member (Reader Config) r
  ) =>
  Log ->
  Sem r ()
handleOutputLog log@(Log tag msg) = do
  Verbosity tags <- asks verbosity
  when (tag `elem` tags) $
    embed $ printf "%s" (show log)
