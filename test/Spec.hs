import Language.Impe.Executing
import Language.Impe.Grammar
import Language.Impe.Typing

display :: String -> String -> IO ()
display tag msg = do
  putStrLn . unlines $
    [ "[" ++ tag ++ "]",
      "",
      msg,
      ""
    ]

{-
TODO: scope error

the following returns Just (Int 10) rather than Just (Bool True)

  Block
    [ Declaration (Name "x") BoolType,
      Assignment (Name "x") (Bool True),
      Block
        [ Declaration (Name "x") IntType,
          Assignment (Name "x") (Int 10)
        ],
      Return (Reference $ Name "x")
    ]

-}

main :: IO ()
main = do
  case runTyping (synthesizeInstruction inst) of
    Left err -> display "typing: error" err
    Right (t, ctx) -> do
      display "typing: success" (show t)
      case runExecuting (executeInstruction inst) of
        Left err -> display "executing: error" err
        Right (e, ctx) -> display "executing: success" (show e)
  where
    inst =
      Block
        [ Declaration (Name "x") BoolType,
          Assignment (Name "x") (Bool True),
          Block
            [ Declaration (Name "x") IntType,
              Assignment (Name "x") (Int 10)
            ],
          Return (Reference $ Name "x")
        ]

-- inst =
--   Block
--     [ Conditional
--         (Bool True)
--         (Return (Bool False))
--         ( Block
--             [ Declaration (Name "x") BoolType,
--               Return (Reference (Name "x"))
--             ]
--         )
--     ]
