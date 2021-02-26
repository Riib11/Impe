{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Lens
import Control.Monad
import Language.Impe.Executing
import Language.Impe.Grammar
import Language.Impe.Parsing
import Language.Impe.Typechecking
import Options.Applicative.Simple
import Polysemy
import Polysemy.Error as Error
import Polysemy.Output as Output
import Polysemy.State as State
import Text.ParserCombinators.Parsec (runParser)
import Text.Printf

_VERBOSITY_PARSING = False

_VERBOSITY_TYPECHECKING = False

_VERBOSITY_EXECUTING = True

main :: IO ()
main = runM $ do
  runError main' >>= \case
    Left err -> embed $ printf "error]\n%s\n" err
    Right () -> return ()

main' :: Sem '[Error String, Embed IO] ()
main' = do
  -- parsing options
  (filename, opts) <- getOptions
  -- reading file
  embed $ printf "[reading] %s\n" filename
  source <- embed $ readFile filename
  -- parsing file
  embed $ printf "[parsing: "
  prog <- case runParser program () filename source of
    Left err -> throw $ show err
    Right prog -> do
      embed do
        printf "success]\n"
        when _VERBOSITY_PARSING $ printf "\n%s\n" (show prog)
      return prog
  -- typechecking program
  embed $ printf "[typechecking: "
  _ <- case execTypecheck (processProgram prog) of
    (logs, Left err) -> throw $ printf "\n%s\n\n%s\n" (show logs) (show err)
    (logs, Right ctx) -> do
      embed do
        printf "success]\n"
        when _VERBOSITY_TYPECHECKING $ printf "\n\n%s\n\n%s\n" (show logs) (show ctx)
      return ctx
  -- executing program
  embed $ printf "[executing: "
  _ <- case execExecution (executeProgram prog) of
    (logs, Left err) -> throw $ printf "\n%s\n\n%s\n" (show logs) (show err)
    (logs, Right ctx) -> do
      embed do
        printf "success]\n"
        when _VERBOSITY_EXECUTING $ printf "\n\n%s\n\n%s\n" (show logs) (show ctx)
      return ctx
  return ()

--

-- TODO: compatiblize with Polysemy
-- main :: IO ()
-- main = do
--   (filename, ()) <-
--     simpleOptions
--       "0.1.0.0"
--       "header"
--       "description"
--       ((strArgument $ metavar "filename") :: Parser String)
--       empty
--   --
--   putStrLn "[read]"
--   source <- readFile filename
--   --
--   putStrLn "[parse]"
--   case runParser program () filename source of
--     Left err -> error $ show err
--     Right prog -> do
--       print prog
--       --
--       putStrLn "[typecheck]"
--       case runTyping (processProgram prog) of
--         Left err -> display "typechecking: error" err
--         Right (t, ctx) -> do
--           display "typechecking: success" (show ctx)
--           --
--           putStrLn "[executing]"
--           case runExecuting (executeProgram prog) of
--             Left (Error msg ctx) -> do
--               display "executing: error" $ unlines [msg, show ctx]
--             Right (e, ctx) -> do
--               display "executing: success" (show ctx)
--               unless (null $ ctx ^. input) $
--                 display "executing: input" (show $ ctx ^. input)
--               unless (null $ ctx ^. output) $
--                 display "executing: output" (show $ ctx ^. output)

display :: String -> String -> IO ()
display tag msg = do
  putStrLn . unlines $
    [ tag,
      "",
      msg
    ]

getOptions :: Sem '[Error String, Embed IO] (String, ())
getOptions =
  embed $
    simpleOptions
      "0.1.0.0"
      "header"
      "description"
      ((strArgument $ metavar "filename") :: Parser String)
      empty
