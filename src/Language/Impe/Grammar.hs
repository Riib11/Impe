module Language.Impe.Grammar where

data Program
  = Program [Statement] Instruction
  deriving (Show, Eq)

data Statement
  = Function Name [(Name, Type)] Type Instruction
  deriving (Show, Eq)

data Instruction
  = Block [Instruction]
  | Declaration Name Type
  | Assignment Name Expression
  | Conditional Expression Instruction Instruction
  | Loop Expression Instruction
  | Return Expression
  deriving (Show, Eq)

data Type
  = UnitType
  | IntType
  | BoolType
  | FunctionType [Type] Type
  deriving (Show, Eq)

data Expression
  = Unit
  | Bool Bool
  | Int Int
  | Reference Name
  | Application Name [Expression]
  deriving (Show, Eq)

type Name = String
