import Language.Impe.Executing
import Language.Impe.Grammar
import Language.Impe.Parsing
import Language.Impe.Typing
import Text.ParserCombinators.Parsec

main :: IO ()
main = mapM_ parse example_filenames
  where
    parse filename = do
      source <- readFile filename
      print $ runParser program () filename source
    example_filenames =
      ("examples/" ++)
        <$> [ "test.imp"
            ]

-- main :: IO ()
-- main = do
--   case runTyping (synthesizeInstruction inst) of
--     Left err -> display "typing: error" err
--     Right (t, ctx) -> do
--       display "typing: success" (show t)
--       case runExecuting (executeInstruction inst) of
--         Left err -> display "executing: error" err
--         Right (e, ctx) -> display "executing: success" (show e)
--   where
--     inst =
--       Block
--         [ Declaration (Name "x") BoolType,
--           Assignment (Name "x") (Bool True),
--           Block
--             [ Declaration (Name "x") IntType,
--               Assignment (Name "x") (Int 10)
--             ],
--           Return (Reference $ Name "x")
--         ]

-- display :: String -> String -> IO ()
-- display tag msg = do
--   putStrLn . unlines $
--     [ "[" ++ tag ++ "]",
--       "",
--       msg,
--       ""
--     ]
