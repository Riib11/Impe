module Language.Impe.Parsing where

import Language.Impe.Grammar
import Language.Impe.Lexing
import Text.ParserCombinators.Parsec

{-
## Program
-}

program :: Parser Program
program = do
  insts <- many instruction
  eof
  return $ Program insts

{-
## Instruction
-}

instruction :: Parser Instruction
instruction = do
  inst <-
    choice . map try $
      [ block,
        return_,
        function,
        conditional,
        loop,
        declaration,
        assignment,
        procedureCall
      ]
  return inst

-- { inst... }
block :: Parser Instruction
block = do
  insts <- braces (many instruction)
  return $ Block insts

-- x : t
declaration :: Parser Instruction
declaration = do
  x <- name
  colon
  t <- type_
  semi
  return $ Declaration x t

-- x <- e
assignment :: Parser Instruction
assignment = do
  x <- name
  symbol "<-"
  e <- expression
  semi
  return $ Assignment x e

-- function f (x1:t1, ...) : t = inst
function :: Parser Instruction
function = do
  f <- name
  params <- (parens . commaSep) do
    x <- name
    colon
    t <- type_
    return (x, t)
  colon
  t <- type_
  eq
  inst <- instruction
  return $ Function f params t inst

-- if e then inst1 else inst2
conditional :: Parser Instruction
conditional = do
  symbol "if"
  e <- expression
  symbol "then"
  inst1 <- instruction
  symbol "else"
  inst2 <- instruction
  return $ Conditional e inst1 inst2

-- while e do inst
loop :: Parser Instruction
loop = do
  symbol "while"
  e <- expression
  symbol "do"
  inst <- instruction
  return $ Loop e inst

-- return e
return_ :: Parser Instruction
return_ = do
  symbol "return"
  e <- expression
  semi
  return $ Return e

-- f(e, ...)
procedureCall :: Parser Instruction
procedureCall = do
  Application f es <- application
  semi
  return $ ProcedureCall f es

{-
## Type
-}

type_ :: Parser Type
type_ =
  choice . map try $
    [ voidType,
      unitType,
      intType,
      boolType,
      functionType
    ]

-- void
voidType :: Parser Type
voidType = do
  symbol "void"
  return VoidType

-- unit
unitType :: Parser Type
unitType = do
  symbol "unit"
  return UnitType

-- int
intType :: Parser Type
intType = do
  symbol "int"
  return IntType

-- bool
boolType :: Parser Type
boolType = do
  symbol "bool"
  return BoolType

-- (s, ...) -> t
functionType :: Parser Type
functionType = do
  params <- parens . commaSep $ type_
  arrow
  t <- type_
  return $ FunctionType params t

{-
## Expression
-}

expression :: Parser Expression
expression =
  choice . map try $
    [ unit,
      bool,
      int,
      application,
      reference,
      parens expression
    ]

-- unit
unit :: Parser Expression
unit = do
  symbol "unit"
  return Unit

-- true | false
bool :: Parser Expression
bool =
  choice
    [ do
        symbol "true"
        return $ Bool True,
      do
        symbol "false"
        return $ Bool False
    ]

int :: Parser Expression
int = do
  i <-
    fromInteger <$> do
      optional (char '-')
      natural
  return $ Int i

-- x
reference :: Parser Expression
reference = do
  x <- name
  return $ Reference x

-- f (e, ...)
application :: Parser Expression
application = do
  f <- name
  args <- parens . commaSep $ expression
  return $ Application f args

{-
## Name
-}

-- x
name :: Parser Name
name = Name <$> identifier

{-
## REPL
-}

data InputREPL
  = InstructionREPL Instruction
  | ExpressionREPL Expression
  | CommandREPL CommandREPL
  deriving (Show)

data CommandREPL
  = CommandREPL_Quit
  | CommandREPL_GetContext
  | CommandREPL_GetType Instruction
  deriving (Show)

inputREPL :: Parser InputREPL
inputREPL = do
  inpt <-
    choice . map try $
      [ CommandREPL <$> commandREPL,
        InstructionREPL <$> instruction,
        ExpressionREPL <$> expression
      ]
  eof
  return inpt

commandREPL :: Parser CommandREPL
commandREPL = do
  char ':'
  choice . map try $
    [ do
        choice . map symbol $ ["q", "quit"]
        return CommandREPL_Quit,
      do
        choice . map symbol $ ["ctx", "context"]
        return CommandREPL_GetContext,
      do
        choice . map symbol $ ["t", "type"]
        inst <- instruction <|> (Return <$> expression)
        return $ CommandREPL_GetType inst
    ]
