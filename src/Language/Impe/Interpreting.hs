module Language.Impe.Interpreting where

import Language.Impe.Excepting as Excepting
import qualified Language.Impe.Executing as Executing
import Language.Impe.Grammar
import Language.Impe.Logging
import Language.Impe.Parsing
import qualified Language.Impe.Typechecking as Typechecking
import Polysemy
import Polysemy.Error (Error)
import Polysemy.Output (Output)
import Polysemy.State
import Text.Printf
import Prelude hiding (log)

{-
# Interpreting
-}

{-
## Computations
-}

type Interpretation r a =
  ( Member (Output Log) r,
    Member (State Typechecking.Context) r,
    Member (State Executing.Context) r,
    Member (Error Exception) r
  ) =>
  Sem r a

{-
### Program
-}

interpretProgram :: String -> String -> Interpretation r ()
interpretProgram filename source = do
  -- parse
  log Tag_InfoInline $ "parsing source"
  prgm <- parseProgram filename source
  log Tag_InfoInline $ printf "parsed program:\n\n%s\n" (show prgm)
  -- typecheck
  log Tag_InfoInline $ "typechecking program"
  Typechecking.typecheckProgram prgm
  tchCtx <- get :: Member (State Typechecking.Context) r => Sem r Typechecking.Context
  log Tag_InfoInline $ printf "typechecked context:\n\n%s\n" (show tchCtx)
  -- execute
  log Tag_InfoInline $ printf "executing program"
  Executing.executeProgram prgm
  exeCtx <- get :: Member (State Executing.Context) r => Sem r Executing.Context
  log Tag_InfoInline $ printf "executed context:\n\n%s\n" (show exeCtx)

{-
### Instruction
-}

interpretInstruction :: String -> String -> Interpretation r (Maybe Value, Type)
interpretInstruction filename source = do
  -- parse
  log Tag_InfoInline $ "parsing source"
  inst <- parseInstruction filename source
  log Tag_InfoInline $ printf "parsed instruction:\n\n%s\n" (show inst)
  --
  interpretInstructionParsed inst

interpretInstructionParsed :: Instruction -> Interpretation r (Maybe Value, Type)
interpretInstructionParsed inst = do
  -- typecheck
  log Tag_InfoInline $ "typechecking instruction"
  t <- Typechecking.synthesizeInstruction inst
  tchCtx <- get :: Member (State Typechecking.Context) r => Sem r Typechecking.Context
  log Tag_InfoInline $ printf "typechecked context:\n\n%s\n" (show tchCtx)
  -- execute
  log Tag_InfoInline $ printf "executing instruction"
  mb_v <- Executing.executeInstruction inst
  exeCtx <- get :: Member (State Executing.Context) r => Sem r Executing.Context
  log Tag_InfoInline $ printf "executed context:\n\n%s\n" (show exeCtx)
  --
  return (mb_v, t)

{-
### Expression
-}

interpretExpression :: String -> String -> Interpretation r (Value, Type)
interpretExpression filename source = do
  -- parse
  log Tag_InfoInline $ "parsing source"
  expr <- parseExpression filename source
  log Tag_InfoInline $ printf "parsed expression:\n\n%s\n" (show expr)
  --
  interpretExpressionParsed expr

interpretExpressionParsed :: Expression -> Interpretation r (Value, Type)
interpretExpressionParsed expr = do
  -- typecheck
  log Tag_InfoInline $ "typechecking expression"
  t <- Typechecking.synthesizeExpression expr
  tchCtx <- get :: Member (State Typechecking.Context) r => Sem r Typechecking.Context
  log Tag_InfoInline $ printf "typechecked context:\n\n%s\n" (show tchCtx)
  -- execute
  log Tag_InfoInline $ printf "executing expression"
  v <- Executing.evaluateExpression expr
  exeCtx <- get :: Member (State Executing.Context) r => Sem r Executing.Context
  log Tag_InfoInline $ printf "executed context:\n\n%s\n" (show exeCtx)
  --
  return (v, t)
