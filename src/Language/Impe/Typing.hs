module Language.Impe.Typing where

import Control.Lens hiding (Context, locally)
import Control.Monad.State as State
import Data.Map as Map
import Language.Impe.Grammar

{-
# Typing

TODO: description
-}

{-
## Typing Computation
-}

type Typing a = StateT Context (Either Error) a

data Context = Context
  { _typings :: Map Name Type
  }

data Error = Error
  { title :: String,
    description :: String
  }

makeLenses ''Context

{-
### Interface
-}

throw :: Error -> Typing a
throw = lift . Left

orElse :: Monad m => m Bool -> m () -> m ()
mb `orElse` m = mb >>= flip unless m

{-
## Typing
-}

checkProgram :: Program -> Typing ()
checkProgram = \case
  Program stmts main -> do
    mapM_ checkStatement stmts
    checkInstruction main UnitType

checkStatement :: Statement -> Typing ()
checkStatement = \case
  -- f (.. xi: si ..) -> t = inst
  Function f params t inst -> do
    setTyping f $ FunctionType (snd <$> params) t -- f: (.. si ..) -> t
    locally do
      void $ mapM (uncurry setTyping) params -- forall i. xi: si
      synthesizeInstruction inst >>= \case
        Just t' -> unifiableTypes t t' `orElse` (throw $ Error "function definition type does not match implementation type." (concat ["the declared return type and implementation return type do not match for function\n  ", show (Function f params t inst), "."]))
        Nothing -> throw $ Error "function implementation does not have a return type." (concat ["for function\n  `", show f, "`\nits implementation\n  `", show inst, "`\ndoes not have a return type when it is expected to have return type\n  `", show t, "`"])

synthesizeInstruction :: Instruction -> Typing (Maybe Type)
synthesizeInstruction = \case
  Block insts ->
    Just <$> synthesizeInstructionBlock insts
  Declaration _ _ ->
    return Nothing
  Assignment _ _ ->
    return Nothing
  Conditional e inst1 inst2 ->
    synthesizeExpression e >>= \case
      BoolType ->
        (,) <$> synthesizeInstruction inst1 <*> synthesizeInstruction inst2 >>= \case
          (Nothing, Nothing) -> return Nothing
          (Just t1, Nothing) -> return . Just $ t1
          (Nothing, Just t2) -> return . Just $ t2
          (Just t1, Just t2) ->
            if t1 == t2
              then return . Just $ t1
              else throw $ Error "mismatching synthesized types from conditional branches" (concat ["the type synthesized from branch\n  ", show inst1, "\ndoes not match the type synthesized from branch\b  `", show inst2, "`"])
      _ -> throw $ Error "condition expression does not have type `Bool`" (concat ["the conditional expression\n  `", show e, "`\ndoes not have type `Bool`"])
  Loop e inst -> do
    checkExpression e BoolType
    synthesizeInstruction inst
  Return e ->
    Just <$> synthesizeExpression e

synthesizeInstructionBlock :: [Instruction] -> Typing Type
synthesizeInstructionBlock [] = throw $ Error "no `return` instruction in block" "no `return` instruction in block"
synthesizeInstructionBlock [inst] =
  synthesizeInstruction inst >>= \case
    Just t -> return t
    Nothing -> throw $ Error "no `return` instruction in block" (concat ["no `return` instruction in block that ends with\n  `", show inst, "`"])
synthesizeInstructionBlock (inst : insts) = do
  t' <- synthesizeInstructionBlock insts
  synthesizeInstruction inst >>= \case
    Just t -> do
      unifiableTypes t t' `orElse` (throw $ Error "mismatching return types in instruction block" (concat ["the instruction\n  `", show inst, "`\ndoes having a type matching that of it's following instructions\n  `", show insts, "`"]))
      return t
    Nothing -> return t'

synthesizeExpression :: Expression -> Typing Type
synthesizeExpression = \case
  Unit ->
    return UnitType
  Bool _ ->
    return BoolType
  Int _ ->
    return IntType
  Reference x ->
    getTyping x
  Application f es ->
    getTyping f >>= \case
      FunctionType ss t -> do
        mapM_ (uncurry checkExpression) (zip es ss)
        return t
      fType -> throw $ Error "invalid application of non-function" (concat ["the applicant\n`", show f, "`\nhas type\n  `", show fType, "`\nrather than a function type."])

checkInstruction :: Instruction -> Type -> Typing ()
checkInstruction inst t =
  synthesizeInstruction inst >>= \case
    Just t' -> unifiableTypes t t' `orElse` (throw $ Error "instruction has unexpected return type" (concat ["expected instruction\n  `", show inst, "`\nto have return type\n  `", show t, "`\nbut it actually has return type\n  `", show t', "`"]))
    Nothing -> throw $ Error "instruction unexpectedly has ne return type" (concat ["the instruction\n  `", show inst, "`\n has no return type rather than expected return type\n  `", show t, "`"])

checkExpression :: Expression -> Type -> Typing ()
checkExpression e t = do
  t' <- synthesizeExpression e
  unifiableTypes t t' `orElse` (throw $ Error "expression has unexpected type" (concat ["expected expression\n  `", show e, "`\nto have type\n  `", show t, "`\nbut it actually has type\n  `", show t', "`"]))

unifyExpressionTypes :: Expression -> Expression -> Typing ()
unifyExpressionTypes e1 e2 = do
  t1 <- synthesizeExpression e1
  t2 <- synthesizeExpression e2
  unifiableTypes t1 t2 `orElse` (throw $ Error "expression types do not unify" (concat ["expected expressions\n  `", show e1, "`\nand\n  `", show e2, "`\nto have unifiable types, but instead they have types\n  `", show t1, "`\nand\n  `", show t2, "`\nrespectively."]))

-- simple for now
unifiableTypes :: Type -> Type -> Typing Bool
unifiableTypes s t = return $ s == t

locally :: Typing a -> Typing a
locally c = do
  st <- get
  (a, _) <- lift $ runStateT c st
  return a

setTyping :: Name -> Type -> Typing ()
setTyping x t =
  typings . at x .= Just t -- shadows previous typings of x

getTyping :: Name -> Typing Type
getTyping x =
  use (typings . at x) >>= \case
    Just t -> return t
    Nothing -> throw $ Error "undeclared name" (concat ["the name\n  `", x, "`\n is not declared before it is mentioned."])
