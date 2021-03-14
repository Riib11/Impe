module Language.Impe.Typechecking
  ( Context (..),
    emptyContext,
    typecheckProgram,
    typecheckInstruction,
    typecheckExpression,
    synthesizeInstruction,
    synthesizeInstructionStep,
    synthesizeExpression,
  )
where

import Control.Lens hiding (Context)
import Control.Monad
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import Data.Namespace as Namespace
import qualified Language.Impe.Excepting as Excepting
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
    Member (Error Excepting.Exception) r,
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
          . map (\(uid, t) -> printf "    %s: %s" (show uid) (show t))
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
    log Tag_Debug "typecheck program"
    typecheckPrelude
    mapM_ (flip typecheckInstruction VoidType) insts
    typecheckMain

typecheckPrelude :: Typecheck r ()
typecheckPrelude = do
  log Tag_Debug "typecheck prelude"
  mapM_
    (\(x, t, _) -> setType x t)
    primitive_variables
  mapM_
    (\(f, ss, t) -> setType f $ FunctionType ss t)
    primitive_functions

typecheckMain :: Typecheck r ()
typecheckMain = do
  log Tag_Debug "typecheck main"
  gets (^. namespace . at mainName) >>= \case
    Just t -> when (t /= mainType) . throw $ Excepting.MainType
    Nothing -> return ()

{-
## Checking
-}

typecheckInstruction :: Instruction -> Type -> Typecheck r ()
typecheckInstruction inst t = do
  log Tag_Debug "typecheck instruction"
  t' <- synthesizeInstruction inst
  void $ typecheckTypes t t'

typecheckExpression :: Expression -> Type -> Typecheck r ()
typecheckExpression e t = do
  log Tag_Debug "typecheck expression"
  t' <- synthesizeExpression e
  void $ typecheckTypes t t'

{-
## Synthesizing
-}

synthesizeInstruction :: Instruction -> Typecheck r Type
synthesizeInstruction inst = do
  log Tag_Debug "synthesize instruction"
  synthesizeInstructionStep inst >>= \case
    Just t -> return t
    Nothing -> return VoidType

synthesizeInstructionStep :: Instruction -> Typecheck r (Maybe Type)
synthesizeInstructionStep inst_ = case inst_ of
  Block insts -> withLocalScope do
    log Tag_Debug "synthesize block"
    ts <- mapM synthesizeInstructionStep insts
    foldM typecheckIntermediateTypes Nothing ts
  Declaration x t -> do
    log Tag_Debug $ printf "synthesize declaration: %s" (show inst_)
    when (t == VoidType) . throw $ Excepting.VariableVoid x
    setType x t
    return Nothing
  Assignment x e -> do
    log Tag_Debug $ printf "synthesize assignment: %s" (show inst_)
    t <- getType x
    t' <- synthesizeExpression e
    void $ typecheckTypes t t'
    return Nothing
  Initialization x t e -> do
    log Tag_Debug $ printf "synthesize initialization: %s" (show inst_)
    -- declaration
    when (t == VoidType) . throw $ Excepting.VariableVoid x
    setType x t
    -- assignment
    t' <- synthesizeExpression e
    void $ typecheckTypes t t'
    return Nothing
  Function f prms t inst -> do
    log Tag_Debug $ printf "synthesize function: %s" (show inst_)
    setType f $ FunctionType (snd <$> prms) t
    withLocalScope do
      mapM_ (\(x, s) -> setType x s) prms
      typecheckInstruction inst t
    return Nothing
  Branch e inst1 inst2 -> do
    log Tag_Debug $ printf "synthesize branch: %s" (show inst_)
    typecheckExpression e BoolType
    mbt1 <- withLocalScope $ synthesizeInstructionStep inst1
    mbt2 <- withLocalScope $ synthesizeInstructionStep inst2
    typecheckIntermediateTypes mbt1 mbt2
  Loop e inst -> do
    log Tag_Debug $ printf "synthesize loop: %s" (show inst_)
    typecheckExpression e BoolType
    withLocalScope $ synthesizeInstructionStep inst
  Return e -> do
    log Tag_Debug $ printf "synthesize return: %s" (show inst_)
    Just <$> synthesizeExpression e
  ProcedureCall f args -> do
    log Tag_Debug $ printf "synthesize procedure call: %s" (show inst_)
    getType f >>= \case
      fType@(FunctionType ss t) -> do
        unless (length args == length ss) . throw $
          Excepting.ApplicationArgumentsNumber f fType (length ss) args
        mapM_ (uncurry typecheckExpression) (zip args ss)
        return t
      fType ->
        throw $ Excepting.ApplicationNonfunction f fType args
    return Nothing
  Pass ->
    return Nothing

synthesizeExpression :: Expression -> Typecheck r Type
synthesizeExpression e_ = case e_ of
  Unit -> do
    log Tag_Debug $ printf "synthesize unit: %s" (show e_)
    return UnitType
  Bool _ -> do
    log Tag_Debug $ printf "synthesize bool: %s" (show e_)
    return BoolType
  Int _ -> do
    log Tag_Debug $ printf "synthesize int: %s" (show e_)
    return IntType
  String _ -> do
    log Tag_Debug $ printf "synthesize string: %s" (show e_)
    return StringType
  Reference x -> do
    log Tag_Debug $ printf "synthesize reference: %s" (show e_)
    getType x
  Application f args -> do
    log Tag_Debug $ printf "synthesize application: %s" (show e_)
    getType f >>= \case
      fType@(FunctionType ss t) -> do
        unless (length args == length ss) . throw $
          Excepting.ApplicationArgumentsNumber f fType (length ss) args
        mapM_ (uncurry typecheckExpression) (zip args ss)
        return t
      fType ->
        throw $ Excepting.ApplicationNonfunction f fType args

{-
## Unification
-}

typecheckIntermediateTypes :: Maybe Type -> Maybe Type -> Typecheck r (Maybe Type)
typecheckIntermediateTypes mb_t1 mb_t2 = do
  log Tag_Debug $
    printf "typecheck intermediate types: %s ~ %s" (show mb_t1) (show mb_t2)
  case (mb_t1, mb_t2) of
    (Nothing, Nothing) -> return Nothing
    (Nothing, Just t) -> return $ Just t
    (Just s, Nothing) -> return $ Just s
    (Just s, Just t) -> Just <$> typecheckTypes s t

typecheckTypes :: Type -> Type -> Typecheck r Type
typecheckTypes s t = do
  log Tag_Debug $ printf "typecheck types: %s ~ %s" (show s) (show t)
  if s == t
    then return s
    else throw $ Excepting.TypeIncompatibility s t

{-
## Namespace
-}

withLocalScope :: Typecheck r a -> Typecheck r a
withLocalScope tch = do
  log Tag_Debug $ printf "entering local scope"
  modify $ namespace %~ enterLocalScope
  a <- tch
  log Tag_Debug $ printf "leaving local scope"
  modify $ namespace %~ leaveLocalScope
  return a

getType :: Name -> Typecheck r Type
getType n =
  gets (^. namespace . at n) >>= \case
    Just t -> return t
    Nothing -> throw $ Excepting.UndeclaredReference n

setType :: Name -> Type -> Typecheck r ()
setType n t = modify $ namespace %~ initialize n t

{-
## Excepting
-}

throw ::
  Member (Error Excepting.Exception) r =>
  Excepting.Typechecking ->
  Sem r a
throw = Excepting.throw . Excepting.Typechecking
