module Language.Impe.Primitive where

import Language.Impe.Grammar

primitive_variables :: [(Name, Type, Expression)]
primitive_variables =
  []

primitive_functions :: [(Name, Type, [Name])]
primitive_functions =
  [ (Name "&&", FunctionType [BoolType, BoolType] BoolType, Name <$> ["p", "q"]),
    (Name "||", FunctionType [BoolType, BoolType] BoolType, Name <$> ["p", "q"]),
    (Name "print_bool", FunctionType [BoolType] UnitType, Name <$> ["v"]),
    (Name "print_int", FunctionType [IntType] UnitType, Name <$> ["v"])
  ]
