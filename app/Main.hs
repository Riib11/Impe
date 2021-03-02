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
    Left err -> putStrLn err
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

startInterpret :: Sem '[Reader MainOptions, Fail, Embed IO] ()
startInterpret = do
  -- filename
  fn <-
    asks input_filename >>= \case
      Nothing ->
        fail $ printf "[options error] options must provide an INPUT in order to use interpret mode"
      Just fn -> return fn
  -- read
  log Phase_Reading $ printf "reading input file: %s" fn
  src <- embed $ readFile fn
  log Phase_Reading $ printf "read source:\n\n%s\n" src
  -- parse
  log Phase_Parsing $ printf "parsing source"
  prgm <- case runParser program () fn src of
    Left prsErr -> fail $ printf "[parsing error] %s" (show prsErr)
    Right prgm -> return prgm
  log Phase_Parsing $ printf "parsed program:\n\n%s\n" (show prgm)
  -- interpret
  log Phase_Interpreting $ printf "interpreting program"
  itpCtx <-
    (raise_ . runOutputList . runError . execState Interpreting.emptyContext)
      (interpretProgram prgm)
      >>= \case
        (logs, Left err) -> fail err
        (logs, Right itpCtx) -> return itpCtx
  log Phase_Interpreting $ printf "interpreted context:\n\n%s\n" (show itpCtx)
  -- output
  embed . putStr . unlines $ itpCtx ^. executionContext . outputs
  -- done
  return ()

startInteractive :: Sem '[Reader MainOptions, Fail, Embed IO] ()
startInteractive = return () -- TODO

log :: Phase -> String -> Sem '[Reader MainOptions, Fail, Embed IO] ()
log phs msg =
  elem phs <$> asks verbosity >>= \case
    True -> embed . putStrLn $ printf "[%s] %s" (show phs) msg
    False -> return ()
