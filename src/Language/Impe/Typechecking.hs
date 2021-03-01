module Language.Impe.Typechecking where

import Control.Lens hiding (Context)
import Control.Monad
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import Data.Namespace as Namespace
import Language.Impe.Grammar as Grammar
import Language.Impe.Primitive
import Polysemy
import Polysemy.Error as Error
import Polysemy.Output as Output
import Polysemy.State as State
import Text.Printf

{-
# Typechecking

TODO: description
-}

{-
## Data
-}

data Context = Context
  { _namespace :: Namespace Name Type
  }

type Typecheck a =
  Sem
    '[ State Context,
       Error String,
       Output String
     ]
    a

makeLenses ''Context

-- instances

instance Show Context where
  show ctx =
    unlines
      [ "typechecking context:",
        "  namespace:",
        "    scope:",
        printf "      %s" (show $ ctx ^. namespace . scope . to NonEmpty.toList),
        "    store:",
        unlines
          . map (\((x, i), t) -> printf "      %s#%s: %s" (show x) (show i) (show t))
          $ ctx ^. namespace . store . to Map.toList
      ]

{-
## Interface
-}

emptyContext :: Context
emptyContext =
  Context
    { _namespace = mempty
    }

{-
## Typecheck computations
-}

typecheckProgram :: Program -> Typecheck ()
typecheckProgram = \case
  Program insts -> do
    typecheckPrelude
    mapM_ (flip typecheckInstruction VoidType) insts
    typecheckMain

typecheckPrelude :: Typecheck ()
typecheckPrelude = do
  mapM_
    (\(x, t) -> setType x t)
    primitive_variables
  mapM_
    (\(f, prms, t) -> setType f $ FunctionType (snd <$> prms) t)
    primitive_functions

typecheckMain :: Typecheck ()
typecheckMain =
  gets (^. namespace . at mainName) >>= \case
    Just (FunctionType [] VoidType) -> return ()
    Just (FunctionType _ VoidType) -> throw $ printf "the function `main` cannot have any arguments"
    Just (FunctionType _ _) -> throw $ printf "function `main` cannot cannot have non-void return type"
    Just _ -> throw $ printf "the function `main` must be a function type that returns void"
    Nothing -> return ()

{-
## Checking
-}

typecheckInstruction :: Instruction -> Type -> Typecheck ()
typecheckInstruction inst t = do
  t' <- synthesizeInstruction inst
  void $ typecheckTypes t t'

typecheckExpression :: Expression -> Type -> Typecheck ()
typecheckExpression e t = do
  t' <- synthesizeExpression e
  void $ typecheckTypes t t'

{-
## Synthesizing
-}

synthesizeInstruction :: Instruction -> Typecheck Type
synthesizeInstruction inst =
  synthesizeInstructionStep inst >>= \case
    Just t -> return t
    Nothing -> return VoidType

synthesizeInstructionStep :: Instruction -> Typecheck (Maybe Type)
synthesizeInstructionStep = \case
  Block insts -> subscope do
    ts <- mapM synthesizeInstructionStep insts
    foldM typecheckIntermediateTypes Nothing ts
  Declaration x t -> do
    when (t == VoidType) $
      throw $ printf "cannot declare variable `%s` to be of type `void`"
    setType x t
    return Nothing
  Assignment x e -> do
    t <- getType x
    t' <- synthesizeExpression e
    void $ typecheckTypes t t'
    return Nothing
  Function f prms t inst -> do
    setType f $ FunctionType (snd <$> prms) t
    subscope do
      mapM_ (\(x, s) -> setType x s) prms
      typecheckInstruction inst t
    return Nothing
  Conditional e inst1 inst2 -> do
    typecheckExpression e BoolType
    mbt1 <- subscope $ synthesizeInstructionStep inst1
    mbt2 <- subscope $ synthesizeInstructionStep inst2
    typecheckIntermediateTypes mbt1 mbt2
  Loop e inst -> do
    typecheckExpression e BoolType
    subscope $ synthesizeInstructionStep inst
  Return e ->
    Just <$> synthesizeExpression e
  ProcedureCall f es -> do
    getType f >>= \case
      FunctionType ss t -> do
        unless (length es == length ss) $
          throw $ printf "cannot apply function\n\n  %s\n\nof type\n\n  %s\n\nto mismatching number of arguments\n\n  %s(%s)\n\n" (show f) (show $ FunctionType ss t) (show f) (List.intercalate ", " . map show $ es)
        mapM_ (uncurry typecheckExpression) (zip es ss)
        return t
      fType -> throw $ printf "cannot apply reference\n\n  %s\n\nof non-function type\n\n  %s\n\nto arguments\n\n  %s\n\n" (show f) (show fType) (show es)
    return Nothing
  PrimitiveFunctionBody f xs ->
    error $ printf "the type `%s` should not arise from source code" (show $ PrimitiveFunctionBody f xs)

synthesizeExpression :: Expression -> Typecheck Type
synthesizeExpression = \case
  Unit ->
    return UnitType
  Bool _ ->
    return BoolType
  Int _ ->
    return IntType
  Reference x ->
    getType x
  Application f es ->
    getType f >>= \case
      FunctionType ss t -> do
        unless (length es == length ss) $
          throw $ printf "cannot apply function\n\n  %s\n\nof type\n\n  %s\n\nto mismatching number of arguments\n\n  %s(%s)\n\n" (show f) (show $ FunctionType ss t) (show f) (List.intercalate ", " . map show $ es)
        mapM_ (uncurry typecheckExpression) (zip es ss)
        return t
      fType -> throw $ printf "cannot apply reference\n\n  %s\n\nof non-function type\n\n  %s\n\nto arguments\n\n  %s(%s)\n\n" (show f) (show fType) (show f) (List.intercalate ", " . map show $ es)

{-
## Unification
-}

typecheckIntermediateTypes :: Maybe Type -> Maybe Type -> Typecheck (Maybe Type)
typecheckIntermediateTypes Nothing Nothing = return Nothing
typecheckIntermediateTypes Nothing (Just t) = return $ Just t
typecheckIntermediateTypes (Just s) Nothing = return $ Just s
typecheckIntermediateTypes (Just s) (Just t) = Just <$> typecheckTypes s t

typecheckTypes :: Type -> Type -> Typecheck Type
typecheckTypes s t =
  if s == t
    then return s
    else throw $ printf "cannot unify type\n\n  %s\n\nwith type\n\n  %s\n\n" (show s) (show t)

{-
## Utilities
-}

subscope :: Typecheck a -> Typecheck a
subscope c = do
  modify $ namespace %~ enterScope
  a <- c
  modify $ namespace %~ leaveScope
  return a

getType :: Name -> Typecheck Type
getType n =
  gets (^. namespace . at n) >>= \case
    Just t -> return t
    Nothing -> throw $ printf "the name `%s` cannot be mentioned before its declaration" (show n)

setType :: Name -> Type -> Typecheck ()
setType n t = modify $ namespace . at n .~ Just t
