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
  putStrLn "[typing]"
  case runTyping (synthesizeInstruction inst) of
    Left err -> display "error" err
    Right (t, ctx) -> display "success" t
  putStrLn "[executing]"
  case runExecuting (executeInstruction inst) of
    Left err -> display "error" err
    Right (e, ctx) -> display "success" e
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
