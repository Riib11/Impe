module Language.Impe.Excepting where

import qualified Language.Impe.Grammar as Grammar
import Polysemy
import Polysemy.Error hiding (throw)
import qualified Polysemy.Error as Error
import Text.Parsec (ParseError)
import Text.Printf

{-
# Excepting
-}

{-
## Data
-}

data Exception
  = Parsing ParseError
  | Typechecking Typechecking
  | Executing Executing
  | Interpreting Interpreting
  | Misc String

instance Show Exception where
  show = \case
    Parsing prsErr -> show prsErr
    Typechecking tchErr -> printf "[typecheck error] %s\n" (show tchErr)
    Executing exeErr -> printf "[execution error] %s\n" (show exeErr)
    Interpreting itpErr -> printf "[interpretation error] %s\n" (show itpErr)
    Misc msg -> printf "[exception] miscellaneous\n%s" msg

{-
### Typechecking
-}

data Typechecking
  = MainType
  | VariableVoid Grammar.Name
  | ApplicationArgumentsNumber Grammar.Name Grammar.Type Int [Grammar.Expression]
  | ApplicationNonfunction Grammar.Name Grammar.Type [Grammar.Expression]
  | TypeIncompatibility Grammar.Type Grammar.Type
  | UndeclaredReference Grammar.Name

instance Show Typechecking where
  show = \case
    MainType -> printf "the function `main` must be a function of type `%s`" (show $ Grammar.FunctionType [] Grammar.VoidType)
    VariableVoid x -> printf "variables, such as %s, must have a non-`void` type" (show x)
    ApplicationArgumentsNumber f fType nparams args -> printf "the function `%s` of type `%s` has `%i` parameters, yet is given the arguments `(%s)`" (show f) (show fType) nparams (Grammar.showArgs args)
    ApplicationNonfunction f fType args -> printf "the reference `%s` of type `%s` must be a function in order to be applied to arguments `%s`" (show f) (show fType) (show args)
    TypeIncompatibility s t -> printf "expected the types `%s` and `%s` to be compatible, yet they are not" (show s) (show t)
    UndeclaredReference n -> printf "the name `%s` must be declared before it is mentioned" (show n)

{-
### Executing
-}

data Executing
  = ValueMaltyped Grammar.Expression Grammar.Type Grammar.Value
  | InstructionNoReturn Grammar.Instruction
  | ExpressionNoValue Grammar.Expression
  | UninterpretedPrimitiveFunction Grammar.Name [Grammar.Expression]
  | VariableNo Grammar.Name
  | VariableUndeclaredMention Grammar.Name
  | VariableUninitializedMention Grammar.Name
  | FunctionUndeclaredMention Grammar.Name
  | FunctionUninitializedMention Grammar.Name
  | FunctionNo Grammar.Name

instance Show Executing where
  show = \case
    ValueMaltyped e t v -> printf "the expression `%s` was checked to have type `%s`, yet it evaluates to `%s`" (show e) (show t) (show v)
    InstructionNoReturn inst -> printf "the instruction `%s` was checked to have a return type, yet it does not return a value" (show inst)
    ExpressionNoValue expr -> printf "the expression `%s` was checked to have a value (i.e. is not of type `void`), yet it does not" (show expr)
    UninterpretedPrimitiveFunction f args -> printf "the primitive function `%s` applied to arguments `%s` does not have an interpretation" (show f) (Grammar.showArgs args)
    VariableNo x -> printf "expected the name `%s` to refer to a variable" (show x)
    VariableUndeclaredMention x -> printf "the variable `%s` must be declared before it can be mentioned" (show x)
    VariableUninitializedMention x -> printf "the variable `%s` must be initialized before it can be mentioned" (show x)
    FunctionUndeclaredMention x -> printf "the function `%s` must be declared before it can be mentioned" (show x)
    FunctionUninitializedMention x -> printf "the function `%s` must be initialized before it can be mentioned" (show x)
    FunctionNo f -> printf "expected the name `%s` to refer to a (constructed) function" (show f)

{-
### Interpreting
-}

data Interpreting
  = ExpressionVoid Grammar.Expression

instance Show Interpreting where
  show = \case
    ExpressionVoid e -> printf "the expression `%s` was expected to have a value (i.e. is not of type `void`), yet it does not" (show e)

{-
## Excepting computation
-}

throw ::
  Member (Error Exception) r =>
  Exception ->
  Sem r a
throw = Error.throw
