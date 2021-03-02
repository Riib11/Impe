{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

import Control.Lens
import Control.Monad
import Data.List (intercalate)
import Data.Version (showVersion)
import Development.GitRev (gitHash)
import Language.Impe.Executing as Executing
import Language.Impe.Grammar as Grammar
import Language.Impe.Parsing as Parsing
import Language.Impe.Typechecking as Typechecking
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

main :: IO ()
main = do
  let fn = "examples/test.impe"
  interpretFile fn

interpretFile :: String -> IO ()
interpretFile fn = do
  src <- readFile fn
  --
  putStrLn "[parsing]"
  prgm <- case runParser Parsing.program () fn src of
    Left prsErr -> error $ show prsErr
    Right prgm -> return prgm
  print prgm
  --
  putStrLn "[typechecking]"
  tchCtx <- case run . runOutputList . runError . execState Typechecking.emptyContext $ typecheckProgram prgm of
    (logs, Left tcErr) -> error tcErr
    (logs, Right tchCtx) -> return tchCtx
  print tchCtx
  --
  putStrLn "[executing]"
  exeCtx <- case run . runOutputList . runError . execState Executing.emptyContext $ executeProgram prgm of
    (logs, Left exErr) -> error exErr
    (logs, Right exeCtx) -> return exeCtx
  print exeCtx
  return ()
