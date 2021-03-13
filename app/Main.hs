module Main where

import Control.Lens
import Control.Monad
import qualified Language.Impe.Executing as Executing
import Language.Impe.Interpreting
import Language.Impe.Logging
import qualified Language.Impe.Typechecking as Typechecking
import Main.Config.Grammar
import Main.Config.Parsing
import Main.Excepting (Exception, throw)
import qualified Main.Excepting as Excepting
import Main.Interacting
import Main.Output
import Polysemy hiding (interpret)
import Polysemy.Error (Error, runError)
import Polysemy.Output
import Polysemy.Reader
import Polysemy.State
import Polysemy.Writer
import System.IO hiding (interact)
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
    Mode_Interpret -> runReader cfg startInterpret
    Mode_Interact -> runReader cfg startInteract

startInterpret ::
  Sem
    '[ Reader Config,
       Error Exception,
       Embed IO
     ]
    ()
startInterpret = do
  -- get source filename
  src_fn <-
    asks source_filename >>= \case
      Just src_fn -> return src_fn
      Nothing -> throw . Excepting.Config $ Excepting.MissingSource
  -- read source
  src <- embed $ readFile src_fn
  -- read data
  inp <-
    asks input_filename >>= \case
      Just inp_fn -> lines <$> embed (readFile inp_fn)
      Nothing -> return []
  (out, result) <-
    runWriter
      . runError
      . evalState (Executing.initContext inp)
      . evalState Typechecking.emptyContext
      . runOutputSem
        handleOutputLog
      $ do
        -- interpret source
        interpretProgram src_fn src
        -- tell outputs
        Executing.tellOutputString
  -- write output
  writeOutput out
  -- handle exception
  case result of
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
  (src_fn, src) <-
    asks source_filename >>= \case
      -- get filename
      Just src_fn -> do
        src <- embed $ readFile src_fn
        return (src_fn, src)
      -- default empty source file
      Nothing ->
        return ("default_empty_source_file", "")
  -- read data
  inp <-
    asks input_filename >>= \case
      Just inp_fn -> lines <$> embed (readFile inp_fn)
      Nothing -> return []
  -- interpret then interact
  result <-
    runError $ do
      -- interpret
      (out, (exeCtx, (tchCtx, ()))) <-
        runWriter
          . runState (Executing.initContext inp)
          . runState Typechecking.emptyContext
          . runOutputSem
            handleOutputLog
          $ do
            -- interpret source
            interpretProgram src_fn src
            -- tell outputs
            Executing.tellOutputString
            Executing.resetOutputString
      -- write output
      writeOutput out
      -- interact
      evalState exeCtx
        . evalState tchCtx
        . runOutputSem
          handleOutputLog
        $ do
          -- start interact loop
          log Tag_Output "[impe - interact] start"
          interact
  -- handle exception
  case result of
    Left excp -> throw . Excepting.Interpretation $ excp
    Right () -> return ()

handleOutputLog ::
  ( Member (Reader Config) r,
    Member (Embed IO) r
  ) =>
  Log ->
  Sem r ()
handleOutputLog lg@(Log tag _) = do
  Verbosity tags <- asks verbosity
  when (tag `elem` tags) . embed $
    printf "%s" (show lg)
