module Language.Impe.Typing where

import Control.Lens hiding (Context, set)
import Control.Monad.State hiding (get)
import qualified Control.Monad.State as State (get)
import Data.Map as Map hiding (map)
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
  { _variables :: Map Name Type,
    _functions :: Map Name Type
  }

type Error = String

makeLenses ''Context

instance Show Context where
  show ctx =
    unlines
      [ "typing context:",
        "  variables:",
        unlines . map (\(x, t) -> printf "    %s: %s" (show x) (show t)) . toList $ ctx ^. variables,
        "  functions:",
        unlines . map (\(f, t) -> printf "    %s: %s" (show f) (show t)) . toList $ ctx ^. functions
      ]

{-
### Interface
-}

runTyping :: Typing a -> Either Error (a, Context)
runTyping c = runStateT c emptyContext

emptyContext :: Context
emptyContext =
  Context
    { _variables = Map.empty,
      _functions = Map.empty
    }

type_error :: Error -> Typing a
type_error = lift . Left

{-
## Processing
-}

processProgram :: Program -> Typing ()
processProgram = \case
  Program insts -> do
    processPrelude
    mapM_ (flip checkInstruction VoidType) insts
    processMain

processPrelude :: Typing ()
processPrelude = do
  mapM_
    ( \(x, t) ->
        set variables x t
    )
    primitive_variables
  mapM_
    ( \(f, params, t) ->
        set functions f (FunctionType (snd <$> params) t)
    )
    primitive_functions

-- return . unsafePerformIO $ print (f, t)

processMain :: Typing ()
processMain =
  use (functions . at mainName) >>= \case
    Just (FunctionType [] VoidType) -> return ()
    Just (FunctionType _ VoidType) -> type_error $ printf "The function `main` cannot have any arguments."
    Just (FunctionType _ _) -> type_error $ printf "The function `main` cannot cannot have non-void return type."
    Just _ -> type_error $ printf "The function `main` must be a function type that returns void."
    Nothing -> return ()

{-
## Checking
-}

checkInstruction :: Instruction -> Type -> Typing ()
checkInstruction inst t = do
  t' <- synthesizeInstruction inst
  void $ unifyTypes t t'

checkExpression :: Expression -> Type -> Typing ()
checkExpression e t = do
  t' <- synthesizeExpression e
  void $ unifyTypes t t'

{-
## Synthesizing
-}

synthesizeInstruction :: Instruction -> Typing Type
synthesizeInstruction inst =
  synthesizeInstructionStep inst >>= \case
    Just t -> return t
    Nothing -> return VoidType

synthesizeInstructionStep :: Instruction -> Typing (Maybe Type)
synthesizeInstructionStep = \case
  Block insts -> newScope do
    ts <- mapM synthesizeInstructionStep insts
    foldM unifyStepTypes Nothing ts
  Declaration x t -> do
    when (t == VoidType) $
      type_error $ printf "cannot declare variable `%s` to be of type `void`."
    set variables x t
    return Nothing
  Assignment x e -> do
    t <- get variables x
    t' <- synthesizeExpression e
    void $ unifyTypes t t'
    return Nothing
  Function f params t inst -> do
    set functions f $ FunctionType (snd <$> params) t
    newScope do
      mapM_ (uncurry (set variables)) params
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
    get functions f >>= \case
      FunctionType ss t -> do
        unless (length es == length ss) $
          type_error $ printf "cannot apply function\n\n  %s\n\nof type\n\n  %s\n\n to mismatching number of arguments\n\n  %s\n\n" (show f) (show $ FunctionType ss t) (show es)
        mapM_ (uncurry checkExpression) (zip es ss)
        return t
      fType -> type_error $ printf "cannot apply reference\n\n  %s\n\nof non-function type\n\n  %s\n\nto arguments\n\n  %s\n\n" (show f) (show fType) (show es)
    return Nothing
  PrimitiveFunctionBody f xs ->
    error $ printf "the type `%s` should not arise from source code." (show $ PrimitiveFunctionBody f xs)

synthesizeExpression :: Expression -> Typing Type
synthesizeExpression = \case
  Unit ->
    return UnitType
  Bool _ ->
    return BoolType
  Int _ ->
    return IntType
  Reference x ->
    get variables x
  Application f es ->
    get functions f >>= \case
      FunctionType ss t -> do
        unless (length es == length ss) $
          type_error $ printf "cannot apply function\n\n  %s\n\nof type\n\n  %s\n\n to mismatching number of arguments\n\n  %s\n\n" (show f) (show $ FunctionType ss t) (show es)
        mapM_ (uncurry checkExpression) (zip es ss)
        return t
      fType -> type_error $ printf "cannot apply reference\n\n  %s\n\nof non-function type\n\n  %s\n\nto arguments\n\n  %s\n\n" (show f) (show fType) (show es)

{-
## Unification
-}

unifyStepTypes :: Maybe Type -> Maybe Type -> Typing (Maybe Type)
unifyStepTypes Nothing Nothing = return Nothing
unifyStepTypes Nothing (Just t) = return $ Just t
unifyStepTypes (Just s) Nothing = return $ Just s
unifyStepTypes (Just s) (Just t) = Just <$> unifyTypes s t

unifyTypes :: Type -> Type -> Typing Type
unifyTypes s t =
  if s == t
    then return s
    else type_error $ printf "cannot unify type\n\n  %s\n\nwith type\n\n  %s\n\n" (show s) (show t)

{-
## Utilities
-}

newScope :: Typing a -> Typing a
newScope c = do
  ctx <- State.get
  (a, _) <- lift $ runStateT c ctx
  return a

set :: Lens' Context (Map Name v) -> Name -> v -> Typing ()
set field k v = field . at k .= Just v

get :: Lens' Context (Map Name v) -> Name -> Typing v
get field k =
  use (field . at k) >>= \case
    Just v -> return v
    Nothing -> type_error $ printf "the name `%s` cannot be mentioned before its declaration." (show k)
