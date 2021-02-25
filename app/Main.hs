module Main where

import Control.Lens
import Control.Monad
import Language.Impe.Executing
import Language.Impe.Grammar
import Language.Impe.Parsing
import Language.Impe.Typing
import Options.Applicative.Simple
import Text.ParserCombinators.Parsec (runParser)

main :: IO ()
main = do
  (filename, ()) <-
    simpleOptions
      "0.1.0.0"
      "header"
      "description"
      ((strArgument $ metavar "filename") :: Parser String)
      empty
  --
  putStrLn "[read]"
  source <- readFile filename
  --
  putStrLn "[parse]"
  case runParser program () filename source of
    Left err -> error $ show err
    Right prog -> do
      print prog
      --
      putStrLn "[typecheck]"
      case runTyping (processProgram prog) of
        Left err -> display "typechecking: error" err
        Right (t, ctx) -> do
          display "typechecking: success" (show ctx)
          --
          putStrLn "[executing]"
          case runExecuting (executeProgram prog) of
            Left (Error msg ctx) -> do
              display "executing: error" $ unlines [msg, show ctx]
            Right (e, ctx) -> do
              display "executing: success" (show ctx)
              unless (null $ ctx ^. input) $
                display "executing: input" (show $ ctx ^. input)
              unless (null $ ctx ^. output) $
                display "executing: output" (show $ ctx ^. output)

display :: String -> String -> IO ()
display tag msg = do
  putStrLn . unlines $
    [ "[" ++ tag ++ "]",
      "",
      msg
    ]
