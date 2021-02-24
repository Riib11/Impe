module Language.Impe.Primitive where

import Control.Lens
import Language.Impe.Grammar

primitive_variables :: [(Name, Type)]
primitive_variables =
  []

primitive_functions :: [(Name, [(Name, Type)], Type)]
primitive_functions =
  make
    <$> [ ("&&", [("p", BoolType), ("q", BoolType)], BoolType),
          ("||", [("p", BoolType), ("q", BoolType)], BoolType),
          ("print_bool", [("v", BoolType)], VoidType),
          ("print_int", [("v", IntType)], VoidType)
        ]
  where
    make :: (String, [(String, Type)], Type) -> (Name, [(Name, Type)], Type)
    make (f, params, t) = (Name f, (_1 %~ Name) <$> params, t)
