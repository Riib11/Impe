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
          ("bool_to_string", [BoolType], StringType),
          ("write_bool", [BoolType], VoidType),
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
          ("int_to_string", [IntType], StringType),
          ("write_int", [IntType], VoidType),
          -- string
          ("<>", [StringType, StringType], StringType),
          ("write_string", [StringType], VoidType),
          ("read_string", [], StringType)
        ]
  where
    make :: (String, [Type], Type) -> (Name, [Type], Type)
    make (f, xs, t) = (Name f, xs, t)
