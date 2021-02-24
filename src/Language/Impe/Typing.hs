module Language.Impe.Typing where

import Control.Lens hiding (Context, locally)
import Control.Monad.State as State
import Data.Map as Map
import Language.Impe.Grammar
import Language.Impe.Primitive
import Text.Printf

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
  deriving (Show)

type Error = String

makeLenses ''Context

{-
### Interface
-}

runTyping :: Typing a -> Either Error (a, Context)
runTyping c = runStateT c emptyContext

emptyContext :: Context
emptyContext =
  Context
    { _typings = Map.empty
    }

throw :: Error -> Typing a
throw = lift . Left

{-
## Processing
-}

processProgram :: Program -> Typing ()
processProgram = \case
  Program inst -> do
    mapM_ (\(x, t, _) -> setTyping x t) primitive_variables
    mapM_ (\(f, t, _) -> setTyping f t) primitive_functions
    checkInstruction inst UnitType

{-
## Checking
-}

checkInstruction :: Instruction -> Type -> Typing ()
checkInstruction inst t =
  synthesizeInstruction inst >>= \case
    Just t' -> void $ unifyTypes t t'
    Nothing -> throw $ printf "instruction\n\n  %s\n\nhas no return type when it is expected to have return type\n\n  %s\n\n" (show inst) (show t)

checkExpression :: Expression -> Type -> Typing ()
checkExpression e t = do
  t' <- synthesizeExpression e
  void $ unifyTypes t t'

{-
## Synthesizing
-}

synthesizeInstruction :: Instruction -> Typing (Maybe Type)
synthesizeInstruction = \case
  Block insts -> locally do
    ts <- mapM synthesizeInstruction insts
    foldM unifyReturnTypes Nothing ts
  Declaration x t -> do
    setTyping x t
    return Nothing
  Assignment x e -> do
    t <- getTyping x
    t' <- synthesizeExpression e
    void $ unifyTypes t t'
    return Nothing
  Function f params t inst -> do
    setTyping f $ FunctionType (snd <$> params) t
    locally do
      mapM_ (uncurry setTyping) params
      checkInstruction inst t
    return Nothing
  Conditional e inst1 inst2 -> do
    checkExpression e BoolType
    mbt1 <- locally $ synthesizeInstruction inst1
    mbt2 <- locally $ synthesizeInstruction inst2
    unifyReturnTypes mbt1 mbt2
  Loop e inst -> do
    checkExpression e BoolType
    locally $ synthesizeInstruction inst
  Return e ->
    Just <$> synthesizeExpression e
  FunctionCall f es -> do
    void . synthesizeExpression $ Application f es
    return Nothing
  PrimitiveFunctionBody f xs ->
    error $ printf "`%s` should not arise from source code." (show $ PrimitiveFunctionBody f xs)

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
        unless (length es == length ss) $
          throw $ printf "cannot apply function\n\n  %s\n\nof type\n\n  %s\n\n to mismatching number of arguments\n\n  %s\n\n" (show f) (show $ FunctionType ss t) (show es)
        mapM_ (uncurry checkExpression) (zip es ss)
        return t
      fType -> throw $ printf "cannot apply reference\n\n  %s\n\nof non-function type\n\n  %s\n\nto arguments\n\n  %s\n\n" (show f) (show fType) (show es)

{-
## Unification
-}

unifyReturnTypes :: Maybe Type -> Maybe Type -> Typing (Maybe Type)
unifyReturnTypes Nothing Nothing = return Nothing
unifyReturnTypes Nothing (Just t) = return $ Just t
unifyReturnTypes (Just s) Nothing = return $ Just s
unifyReturnTypes (Just s) (Just t) = Just <$> unifyTypes s t

unifyTypes :: Type -> Type -> Typing Type
unifyTypes s t =
  if s == t
    then return s
    else throw $ printf "cannot unify type\n\n %s\n\nwith type\n\n  %s\n\n" (show s) (show t)

{-
## Utilities
-}

locally :: Typing a -> Typing a
locally c = do
  st <- get
  (a, _) <- lift $ runStateT c st
  return a

setTyping :: Name -> Type -> Typing ()
setTyping x t =
  typings . at x .= Just t

getTyping :: Name -> Typing Type
getTyping x =
  use (typings . at x) >>= \case
    Just t -> return t
    Nothing -> throw $ printf "the name\n\n  %s\n\nis not declared before its mention." (show x)
