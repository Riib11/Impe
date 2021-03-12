module Main where

import Control.Lens
import Control.Monad
import qualified Language.Impe.Executing as Executing
import Language.Impe.Interpreting
import Language.Impe.Logging
import qualified Language.Impe.Typechecking as Typechecking
import Main.Config
import Main.Excepting as Excepting
import Main.Interacting as Interacting
import Polysemy hiding (interpret)
import Polysemy.Error (Error, runError)
import Polysemy.Output
import Polysemy.Reader
import Polysemy.State
import System.IO as IO hiding (interact)
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
      Nothing -> throw . Excepting.Config $ "interpretation mode requires an input source file"
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
      Left excp -> throw . Excepting.Interpretation $ excp
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
        -- log outputs
        Executing.logOutputs
        Executing.resetOutputs
        -- start interact loop
        log Tag_Output "[impe - interact] start"
        interact
    )
    >>= \case
      Left excp -> throw . Excepting.Interaction $ excp
      Right () -> return ()

handleOutputLog ::
  ( Member (Embed IO) r,
    Member (Reader Config) r
  ) =>
  Log ->
  Sem r ()
handleOutputLog lg@(Log tag _) = do
  Verbosity tags <- asks verbosity
  when (tag `elem` tags) . embed $
    printf "%s" (show lg)
