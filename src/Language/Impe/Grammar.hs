module Language.Impe.Grammar where

import Data.List (intercalate)
import Data.Namespace
import Text.Printf

data Program
  = Program [Instruction]
  deriving (Eq)

data Instruction
  = Block [Instruction]
  | Declaration Name Type
  | Assignment Name Expression
  | Function Name [(Name, Type)] Type Instruction
  | Conditional Expression Instruction Instruction
  | Loop Expression Instruction
  | Return Expression
  | ProcedureCall Name [Expression] -- procedures are functions that don't return a value
  | PrimitiveFunctionBody Name [Name] -- placeholder for primitive function bodies
  deriving (Eq)

data Type
  = VoidType
  | UnitType
  | IntType
  | BoolType
  | StringType
  | FunctionType [Type] Type
  deriving (Eq)

mainType :: Type
mainType = FunctionType [] VoidType

data Expression
  = Unit
  | Bool Bool
  | Int Int
  | String String
  | Reference Name
  | Application Name [Expression]
  deriving (Eq)

type Value = Expression

type Binding = ([Name], Instruction)

type Closure = (Binding, Scope Name)

newtype Name
  = Name String
  deriving (Eq, Ord)

mainName :: Name
mainName = Name "main"

{-
## Show instances
-}

instance Show Program where
  show = \case
    Program insts -> intercalate " " . map show $ insts

instance Show Instruction where
  show = \case
    Block insts ->
      "{ " ++ (intercalate " " . map show $ insts) ++ " }"
    Declaration x t ->
      printf "%s: %s;" (show x) (show t)
    Assignment x e ->
      printf "%s <- %s;" (show x) (show e)
    Function x params t inst ->
      printf
        "%s(%s): %s = %s"
        (show x)
        (showParams params)
        (show t)
        (show inst)
    Conditional e inst1 inst2 ->
      printf "if %s then %s else %s" (show e) (show inst1) (show inst2)
    Loop e inst ->
      printf "while %s do %s" (show e) (show inst)
    Return e ->
      printf "return %s;" (show e)
    ProcedureCall f args ->
      printf "%s(%s);" (show f) (showArgs args)
    PrimitiveFunctionBody f xs ->
      printf "[primitive(%s)(%s)]" (show f) (showArgsNames xs)

instance Show Type where
  show = \case
    VoidType -> "void"
    UnitType -> "unit"
    IntType -> "int"
    BoolType -> "bool"
    StringType -> "string"
    FunctionType ss t -> printf "(%s) -> %s" (showArgsTypes ss) (show t)

instance Show Expression where
  show = \case
    Unit -> "unit"
    Bool b -> if b then "true" else "false"
    Int i -> show i
    String s -> show s
    Reference x -> show x
    Application f args -> printf "%s(%s)" (show f) (showArgs args)

instance Show Name where
  show (Name x) = x

{-
### Utilities
-}

showArgs :: [Expression] -> String
showArgs = intercalate ", " . map show

showArgsTypes :: [Type] -> String
showArgsTypes = intercalate ", " . map show

showArgsNames :: [Name] -> String
showArgsNames = intercalate ", " . map show

showParams :: [(Name, Type)] -> String
showParams = intercalate ", " . map (\(x, t) -> printf "%s: %s" (show x) (show t))
