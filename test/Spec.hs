import Language.Impe.Executing
import Language.Impe.Grammar
import Language.Impe.Typing

display :: Show a => String -> a -> IO ()
display tag value = do
  putStrLn . unlines $
    [ "[" ++ tag ++ "]",
      "",
      "  " ++ show value,
      ""
    ]

main :: IO ()
main = do
  case runTyping (synthesizeInstruction inst) of
    Left err -> display "typing: error" err
    Right (t, ctx) -> display "typing: success" t
  case runExecuting (executeInstruction inst) of
    Left err -> display "executing: error" err
    Right (e, ctx) -> display "executing: success" e
  where
    inst =
      Block
        [ Conditional
            (Bool True)
            (Return (Bool False))
            ( Block
                [ Declaration (Name "x") BoolType,
                  Return (Reference (Name "x"))
                ]
            )
        ]
