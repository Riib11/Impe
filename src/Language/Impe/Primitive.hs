module Language.Impe.Primitive
  ( primitive_variables,
    primitive_functions,
  )
where

import Language.Impe.Grammar

primitive_variables :: [(Name, Type, Expression)]
primitive_variables =
  []

primitive_functions :: [(Name, [Type], Type)]
primitive_functions =
  make
    <$> [ -- bool
          ("&&", [BoolType, BoolType], BoolType),
          ("||", [BoolType, BoolType], BoolType),
          ("~", [BoolType], BoolType),
          ("show_bool", [BoolType], StringType),
          -- int
          ("+", [IntType, IntType], IntType),
          ("-", [IntType, IntType], IntType),
          ("*", [IntType, IntType], IntType),
          ("/", [IntType, IntType], IntType),
          ("^", [IntType, IntType], IntType),
          ("%", [IntType, IntType], IntType),
          ("=", [IntType, IntType], BoolType),
          (">", [IntType, IntType], BoolType),
          (">=", [IntType, IntType], BoolType),
          ("<", [IntType, IntType], BoolType),
          ("<=", [IntType, IntType], BoolType),
          ("show_int", [IntType], StringType),
          -- string
          ("<>", [StringType, StringType], StringType),
          ("write", [StringType], VoidType),
          ("read", [], StringType)
        ]
  where
    make :: (String, [Type], Type) -> (Name, [Type], Type)
    make (f, xs, t) = (Name f, xs, t)
