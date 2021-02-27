module Language.Impe.Typechecking where

import Control.Lens
import Control.Monad
import Data.Map as Map hiding (map)
import Language.Impe.Grammar as Grammar
import Language.Impe.Primitive
import Polysemy
import Polysemy.Error as Error
import Polysemy.Output as Output
import Polysemy.State as State
import Text.Printf

{-
# Typecheck

TODO: description
-}

{-
## Typechecking Computation
-}

data TypecheckingContext = TypecheckingContext
  { _variables :: Map Name Type,
    _functions :: Map Name Type
  }

type TypecheckingLog = String

type TypecheckingError = String

type Typecheck a = Sem '[State TypecheckingContext, Error TypecheckingError, Output TypecheckingLog] a

makeLenses ''TypecheckingContext

-- instances

instance Show TypecheckingContext where
  show ctx =
    unlines
      [ "typechecking context:",
        "  variables:",
        unlines . map (\(x, t) -> printf "    %s: %s" (show x) (show t)) . toList $ ctx ^. variables,
        "  functions:",
        unlines . map (\(f, t) -> printf "    %s: %s" (show f) (show t)) . toList $ ctx ^. functions
      ]

{-
### Interface
-}

runTypecheck ::
  Typecheck a ->
  ( [TypecheckingLog],
    Either TypecheckingError (TypecheckingContext, a)
  )
runTypecheck =
  run
    . runOutputList
    . runError
    . runState emptyTypecheckingContext

execTypecheck ::
  Typecheck a ->
  ( [TypecheckingLog],
    Either TypecheckingError TypecheckingContext
  )
execTypecheck =
  run
    . runOutputList
    . runError
    . execState emptyTypecheckingContext

emptyTypecheckingContext :: TypecheckingContext
emptyTypecheckingContext =
  TypecheckingContext
    { _variables = Map.empty,
      _functions = Map.empty
    }

type_error :: TypecheckingError -> Typecheck a
type_error = throw

{-
## Processing
-}

processProgram :: Program -> Typecheck ()
processProgram = \case
  Program insts -> do
    processPrelude
    mapM_ (flip checkInstruction VoidType) insts
    processMain

processPrelude :: Typecheck ()
processPrelude = do
  mapM_
    ( \(x, t) ->
        setTyping variables x t
    )
    primitive_variables
  mapM_
    ( \(f, params, t) ->
        setTyping functions f (FunctionType (snd <$> params) t)
    )
    primitive_functions

-- return . unsafePerformIO $ print (f, t)

processMain :: Typecheck ()
processMain =
  gets (^. functions . at mainName) >>= \case
    Just (FunctionType [] VoidType) -> return ()
    Just (FunctionType _ VoidType) -> type_error $ printf "The function `main` cannot have any arguments."
    Just (FunctionType _ _) -> type_error $ printf "The function `main` cannot cannot have non-void return type."
    Just _ -> type_error $ printf "The function `main` must be a function type that returns void."
    Nothing -> return ()

{-
## Checking
-}

checkInstruction :: Instruction -> Type -> Typecheck ()
checkInstruction inst t = do
  t' <- synthesizeInstruction inst
  void $ unifyTypes t t'

checkExpression :: Expression -> Type -> Typecheck ()
checkExpression e t = do
  t' <- synthesizeExpression e
  void $ unifyTypes t t'

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
  Block insts -> newScope do
    ts <- mapM synthesizeInstructionStep insts
    foldM unifyStepTypes Nothing ts
  Declaration x t -> do
    when (t == VoidType) $
      type_error $ printf "cannot declare variable `%s` to be of type `void`."
    setTyping variables x t
    return Nothing
  Assignment x e -> do
    t <- getTyping variables x
    t' <- synthesizeExpression e
    void $ unifyTypes t t'
    return Nothing
  Function f params t inst -> do
    setTyping functions f $ FunctionType (snd <$> params) t
    newScope do
      mapM_ (uncurry (setTyping variables)) params
      checkInstruction inst t
    return Nothing
  Conditional e inst1 inst2 -> do
    checkExpression e BoolType
    mbt1 <- newScope $ synthesizeInstructionStep inst1
    mbt2 <- newScope $ synthesizeInstructionStep inst2
    unifyStepTypes mbt1 mbt2
  Loop e inst -> do
    checkExpression e BoolType
    newScope $ synthesizeInstructionStep inst
  Return e ->
    Just <$> synthesizeExpression e
  ProcedureCall f es -> do
    getTyping functions f >>= \case
      FunctionType ss t -> do
        unless (length es == length ss) $
          type_error $ printf "cannot apply function\n\n  %s\n\nof type\n\n  %s\n\n to mismatching number of arguments\n\n  %s\n\n" (show f) (show $ FunctionType ss t) (show es)
        mapM_ (uncurry checkExpression) (zip es ss)
        return t
      fType -> type_error $ printf "cannot apply reference\n\n  %s\n\nof non-function type\n\n  %s\n\nto arguments\n\n  %s\n\n" (show f) (show fType) (show es)
    return Nothing
  PrimitiveFunctionBody f xs ->
    error $ printf "the type `%s` should not arise from source code." (show $ PrimitiveFunctionBody f xs)

synthesizeExpression :: Expression -> Typecheck Type
synthesizeExpression = \case
  Unit ->
    return UnitType
  Bool _ ->
    return BoolType
  Int _ ->
    return IntType
  Reference x ->
    getTyping variables x
  Application f es ->
    getTyping functions f >>= \case
      FunctionType ss t -> do
        unless (length es == length ss) $
          type_error $ printf "cannot apply function\n\n  %s\n\nof type\n\n  %s\n\n to mismatching number of arguments\n\n  %s\n\n" (show f) (show $ FunctionType ss t) (show es)
        mapM_ (uncurry checkExpression) (zip es ss)
        return t
      fType -> type_error $ printf "cannot apply reference\n\n  %s\n\nof non-function type\n\n  %s\n\nto arguments\n\n  %s\n\n" (show f) (show fType) (show es)

{-
## Unification
-}

unifyStepTypes :: Maybe Type -> Maybe Type -> Typecheck (Maybe Type)
unifyStepTypes Nothing Nothing = return Nothing
unifyStepTypes Nothing (Just t) = return $ Just t
unifyStepTypes (Just s) Nothing = return $ Just s
unifyStepTypes (Just s) (Just t) = Just <$> unifyTypes s t

unifyTypes :: Type -> Type -> Typecheck Type
unifyTypes s t =
  if s == t
    then return s
    else type_error $ printf "cannot unify type\n\n  %s\n\nwith type\n\n  %s\n\n" (show s) (show t)

{-
## Utilities
-}

newScope :: Typecheck a -> Typecheck a
newScope c = do
  ctx <- get
  (_, x) <- raise $ runState ctx c
  return x

setTyping :: Lens' TypecheckingContext (Map Name v) -> Name -> v -> Typecheck ()
setTyping field k v = modify $ field . at k .~ Just v

getTyping :: Lens' TypecheckingContext (Map Name v) -> Name -> Typecheck v
getTyping field k =
  gets (^. field . at k) >>= \case
    Just v -> return v
    Nothing -> type_error $ printf "the name `%s` cannot be mentioned before its declaration." (show k)
