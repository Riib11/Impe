import Language.Impe.Grammar
import Language.Impe.Typing

main :: IO ()
main =
  case runTyping (synthesizeInstruction inst) of
    Left err -> putStrLn "[error]\n" >> putStrLn err
    Right (t, ctx) -> putStrLn "[success]\n" >> putStrLn ("  " ++ show t)
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
