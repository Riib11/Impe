module Language.Impe.Typechecking where

import Control.Lens hiding (Context)
import Control.Monad
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import Data.Namespace as Namespace
import Language.Impe.Excepting as Excepting
import Language.Impe.Grammar as Grammar
import Language.Impe.Logging as Logging
import Language.Impe.Primitive as Primitive
import Polysemy
import Polysemy.Error (Error)
import Polysemy.Output (Output)
import Polysemy.State
import Text.Printf
import Prelude hiding (log)

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

type Typecheck r a =
  ( Member (State Context) r,
    Member (Error Exception) r,
    Member (Output Log) r
  ) =>
  Sem r a

makeLenses ''Context

-- instances

instance Show Context where
  show ctx =
    List.intercalate "\n" $
      [ "namespace:",
        "  scope:",
        "    "
          ++ ctx
          ^. namespace . scope
            . to NonEmpty.toList
            . to (List.intercalate "\n    " . map (show . Map.elems)),
        "  store:",
        List.intercalate "\n"
          . map (\((x, i), t) -> printf "    %s#%s: %s" (show x) (show i) (show t))
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

typecheckProgram :: Program -> Typecheck r ()
typecheckProgram = \case
  Program insts -> do
    log Tag_InfoInline "typecheck program"
    typecheckPrelude
    mapM_ (flip typecheckInstruction VoidType) insts
    typecheckMain

typecheckPrelude :: Typecheck r ()
typecheckPrelude = do
  log Tag_InfoInline "typecheck prelude"
  mapM_
    (\(x, t) -> setType x t)
    primitive_variables
  mapM_
    (\(f, prms, t) -> setType f $ FunctionType (snd <$> prms) t)
    primitive_functions

typecheckMain :: Typecheck r ()
typecheckMain = do
  log Tag_InfoInline "typecheck main"
  gets (^. namespace . at mainName) >>= \case
    Just (FunctionType [] VoidType) -> return ()
    Just (FunctionType _ VoidType) -> throw . Exception_Misc $ printf "the function `main` cannot have any arguments"
    Just (FunctionType _ _) -> throw . Exception_Misc $ printf "function `main` cannot cannot have non-void return type"
    Just _ -> throw . Exception_Misc $ printf "the function `main` must be a function type that returns void"
    Nothing -> return ()

{-
## Checking
-}

typecheckInstruction :: Instruction -> Type -> Typecheck r ()
typecheckInstruction inst t = do
  log Tag_InfoInline "typecheck instruction"
  t' <- synthesizeInstruction inst
  void $ typecheckTypes t t'

typecheckExpression :: Expression -> Type -> Typecheck r ()
typecheckExpression e t = do
  log Tag_InfoInline "typecheck expression"
  t' <- synthesizeExpression e
  void $ typecheckTypes t t'

{-
## Synthesizing
-}

synthesizeInstruction :: Instruction -> Typecheck r Type
synthesizeInstruction inst = do
  log Tag_InfoInline "synthesize instruction"
  synthesizeInstructionStep inst >>= \case
    Just t -> return t
    Nothing -> return VoidType

synthesizeInstructionStep :: Instruction -> Typecheck r (Maybe Type)
synthesizeInstructionStep inst_ = case inst_ of
  Block insts -> subScope do
    log Tag_InfoInline "synthesize block"
    ts <- mapM synthesizeInstructionStep insts
    foldM typecheckIntermediateTypes Nothing ts
  Declaration x t -> do
    log Tag_InfoInline $ printf "synthesize declaration: %s" (show inst_)
    when (t == VoidType) $
      throw . Exception_Misc $ printf "cannot declare variable `%s` to be of type `void`"
    setType x t
    return Nothing
  Assignment x e -> do
    log Tag_InfoInline $ printf "synthesize assignment: %s" (show inst_)
    t <- getType x
    t' <- synthesizeExpression e
    void $ typecheckTypes t t'
    return Nothing
  Function f prms t inst -> do
    log Tag_InfoInline $ printf "synthesize function: %s" (show inst_)
    setType f $ FunctionType (snd <$> prms) t
    subScope do
      mapM_ (\(x, s) -> setType x s) prms
      typecheckInstruction inst t
    return Nothing
  Conditional e inst1 inst2 -> do
    log Tag_InfoInline $ printf "synthesize conditional: %s" (show inst_)
    typecheckExpression e BoolType
    mbt1 <- subScope $ synthesizeInstructionStep inst1
    mbt2 <- subScope $ synthesizeInstructionStep inst2
    typecheckIntermediateTypes mbt1 mbt2
  Loop e inst -> do
    log Tag_InfoInline $ printf "synthesize loop: %s" (show inst_)
    typecheckExpression e BoolType
    subScope $ synthesizeInstructionStep inst
  Return e -> do
    log Tag_InfoInline $ printf "synthesize return: %s" (show inst_)
    Just <$> synthesizeExpression e
  ProcedureCall f es -> do
    log Tag_InfoInline $ printf "synthesize procedure call: %s" (show inst_)
    getType f >>= \case
      FunctionType ss t -> do
        unless (length es == length ss) $
          throw . Exception_Misc $ printf "cannot apply function\n\n  %s\n\nof type\n\n  %s\n\nto mismatching number of arguments\n\n  %s(%s)\n" (show f) (show $ FunctionType ss t) (show f) (List.intercalate ", " . map show $ es)
        mapM_ (uncurry typecheckExpression) (zip es ss)
        return t
      fType -> throw . Exception_Misc $ printf "cannot apply reference\n\n  %s\n\nof non-function type\n\n  %s\n\nto arguments\n\n  %s\n" (show f) (show fType) (show es)
    return Nothing
  PrimitiveFunctionBody f xs ->
    error $ printf "the type `%s` should not arise from source code" (show $ PrimitiveFunctionBody f xs)

synthesizeExpression :: Expression -> Typecheck r Type
synthesizeExpression e_ = case e_ of
  Unit -> do
    log Tag_InfoInline $ printf "synthesize unit: %s" (show e_)
    return UnitType
  Bool _ -> do
    log Tag_InfoInline $ printf "synthesize bool: %s" (show e_)
    return BoolType
  Int _ -> do
    log Tag_InfoInline $ printf "synthesize int: %s" (show e_)
    return IntType
  Reference x -> do
    log Tag_InfoInline $ printf "synthesize reference: %s" (show e_)
    getType x
  Application f es -> do
    log Tag_InfoInline $ printf "synthesize application: %s" (show e_)
    getType f >>= \case
      FunctionType ss t -> do
        unless (length es == length ss) $
          throw . Exception_Misc $ printf "cannot apply function\n\n  %s\n\nof type\n\n  %s\n\nto mismatching number of arguments\n\n  %s(%s)\n" (show f) (show $ FunctionType ss t) (show f) (List.intercalate ", " . map show $ es)
        mapM_ (uncurry typecheckExpression) (zip es ss)
        return t
      fType -> throw . Exception_Misc $ printf "cannot apply reference\n\n  %s\n\nof non-function type\n\n  %s\n\nto arguments\n\n  %s(%s)\n" (show f) (show fType) (show f) (List.intercalate ", " . map show $ es)

{-
## Unification
-}

typecheckIntermediateTypes :: Maybe Type -> Maybe Type -> Typecheck r (Maybe Type)
typecheckIntermediateTypes mb_t1 mb_t2 = do
  log Tag_InfoInline $ printf "typecheck intermediate types: %s ~ %s" (show mb_t1) (show mb_t2)
  case (mb_t1, mb_t2) of
    (Nothing, Nothing) -> return Nothing
    (Nothing, Just t) -> return $ Just t
    (Just s, Nothing) -> return $ Just s
    (Just s, Just t) -> Just <$> typecheckTypes s t

typecheckTypes :: Type -> Type -> Typecheck r Type
typecheckTypes s t = do
  log Tag_InfoInline $ printf "typecheck types: %s ~ %s" (show s) (show t)
  if s == t
    then return s
    else throw . Exception_Misc $ printf "cannot unify type\n\n  %s\n\nwith type\n\n  %s\n" (show s) (show t)

{-
## Namespace
-}

subScope :: Typecheck r a -> Typecheck r a
subScope c = do
  log Tag_InfoInline $ printf "entering local scope"
  modify $ namespace %~ enterScope
  a <- c
  log Tag_InfoInline $ printf "leaving local scope"
  modify $ namespace %~ leaveScope
  return a

getType :: Name -> Typecheck r Type
getType n =
  gets (^. namespace . at n) >>= \case
    Just t -> return t
    Nothing -> throw . Exception_Misc $ printf "the name `%s` cannot be mentioned before its declaration" (show n)

setType :: Name -> Type -> Typecheck r ()
setType n t = modify $ namespace %~ initialize n t

{-
## Utilties
-}
