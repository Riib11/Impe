module Language.Impe.Primitive where

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
          ("output_bool", [BoolType], VoidType),
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
          ("output_int", [IntType], VoidType),
          -- string
          ("<>", [StringType, StringType], VoidType),
          ("output_string", [StringType], VoidType)
        ]
  where
    make :: (String, [Type], Type) -> (Name, [Type], Type)
    make (f, xs, t) = (Name f, xs, t)
