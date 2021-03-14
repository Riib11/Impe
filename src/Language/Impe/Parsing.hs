module Language.Impe.Parsing
  ( parseProgram,
    parseInstruction,
    parseExpression,
    instruction,
    expression,
  )
where

import Language.Impe.Excepting as Excepting
import Language.Impe.Grammar
import Language.Impe.Lexing
import Polysemy
import Polysemy.Error (Error)
import Text.ParserCombinators.Parsec hiding (string)
import Text.ParserCombinators.Parsec.Expr

{-
# Parsing
-}

type Parsed r a = Member (Error Exception) r => Sem r a

parseProgram :: String -> String -> Parsed r Program
parseProgram = parsed program

parseInstruction :: String -> String -> Parsed r Instruction
parseInstruction = parsed instruction

parseExpression :: String -> String -> Parsed r Expression
parseExpression = parsed expression

parsed :: Parser a -> String -> String -> Parsed r a
parsed parser filename source =
  case runParser (whiteSpace >> parser) () filename source of
    Left prsErr -> throw . Excepting.Parsing $ prsErr
    Right x -> return x

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
    block
      <|> pass
      <|> return_
      <|> try function
      <|> branch
      <|> loop
      <|> try declaration
      <|> try assignment
      <|> try procedureCall
  return inst

-- { inst... }
block :: Parser Instruction
block = Block <$> braces (many instruction)

-- pass
pass :: Parser Instruction
pass = do
  reserved "pass"
  semi
  return Pass

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
  reservedOp "<-"
  e <- expression
  semi
  return $ Assignment x e

-- f (x1:t1, ...) : t = inst
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
  reserved "="
  inst <- instruction
  return $ Function f params t inst

-- if e then inst1 else inst2
branch :: Parser Instruction
branch = do
  reserved "if"
  e <- expression
  reserved "then"
  inst1 <- instruction
  reserved "else"
  inst2 <- instruction
  return $ Branch e inst1 inst2

-- while e do inst
loop :: Parser Instruction
loop = do
  reserved "while"
  e <- expression
  reserved "do"
  inst <- instruction
  return $ Loop e inst

-- return e
return_ :: Parser Instruction
return_ = do
  reserved "return"
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
  voidType
    <|> unitType
    <|> intType
    <|> boolType
    <|> stringType
    <|> functionType

-- void
voidType :: Parser Type
voidType = symbol "void" >> return VoidType

-- unit
unitType :: Parser Type
unitType = symbol "unit" >> return UnitType

-- bool
boolType :: Parser Type
boolType = symbol "bool" >> return BoolType

-- int
intType :: Parser Type
intType = symbol "int" >> return IntType

-- string
stringType :: Parser Type
stringType = symbol "string" >> return StringType

-- (s, ...) -> t
functionType :: Parser Type
functionType = do
  params <- parens . commaSep $ type_
  reserved "->"
  t <- type_
  return $ FunctionType params t

{-
## Expression
-}

expression :: Parser Expression
expression = buildExpressionParser ops expression'
  where
    ops =
      [ [ binaryOp "^" AssocLeft
        ],
        [ binaryOp "*" AssocLeft,
          binaryOp "/" AssocLeft
        ],
        [ binaryOp "+" AssocLeft,
          binaryOp "-" AssocLeft
        ],
        [ binaryOp "%" AssocLeft
        ],
        [ binaryOp "<=" AssocLeft,
          binaryOp "<" AssocLeft,
          binaryOp ">=" AssocLeft,
          binaryOp ">" AssocLeft
        ],
        [ binaryOp "=" AssocLeft
        ],
        [ unaryOp "~"
        ],
        [ binaryOp "&&" AssocLeft,
          binaryOp "||" AssocLeft
        ],
        [ binaryOp "<>" AssocLeft
        ]
      ]
    unaryOp opString =
      Prefix do
        reservedOp opString
        return \a -> Application (Name opString) [a]
    binaryOp opString assocDir =
      flip Infix assocDir do
        reservedOp opString
        return \a b -> Application (Name opString) [a, b]

expression' :: Parser Expression
expression' =
  parens expression
    <|> try unit
    <|> try bool
    <|> try int
    <|> try string
    <|> try application
    <|> try reference

-- unit
unit :: Parser Expression
unit = do
  symbol "unit"
  return Unit

-- true | false
bool :: Parser Expression
bool =
  (symbol "true" >> return (Bool True))
    <|> (symbol "false" >> return (Bool True))

-- [0-9]*
int :: Parser Expression
int = do
  i <-
    fromInteger <$> do
      optional (char '-')
      natural
  return $ Int i

-- "..."
string :: Parser Expression
string = String <$> stringLiteral

-- x
reference :: Parser Expression
reference = Reference <$> name

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
