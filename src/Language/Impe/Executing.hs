module Language.Impe.Executing where

import Control.Applicative
import Control.Lens hiding (Context, set)
import Control.Monad.State as State hiding (get)
import Data.Map as Map
import Language.Impe.Grammar
import Language.Impe.Primitive
import Text.Printf

{-
# Executing

TODO: description
-}

{-
## Executing Computation
-}

type Executing a = StateT Context (Either Error) a

data Context = Context
  { _scopes :: [Scope],
    _input :: [String],
    _output :: [String]
  }
  deriving (Show)

data Scope = Scope
  { _closures :: Map Name (Maybe Closure),
    _variables :: Map Name (Maybe Value)
  }
  deriving (Show)

type Closure = ([Name], Instruction)

type Value = Expression

type Error = String

makeLenses ''Scope
makeLenses ''Context

{-
### Interface
-}

runExecuting :: Executing a -> Either Error (a, Context)
runExecuting c = runStateT c emptyContext

emptyContext :: Context
emptyContext =
  Context
    { _scopes =
        [ Scope
            { _closures = Map.empty,
              _variables = Map.empty
            }
        ],
      _input = [],
      _output = []
    }

emptyScope :: Scope
emptyScope =
  Scope
    { _closures = Map.empty,
      _variables = Map.empty
    }

throw :: Error -> Executing a
throw = lift . Left

type_prohibited :: String -> a
type_prohibited msg = error $ printf "[type-prohibited] %s" msg

impossible :: String -> a
impossible msg = error $ printf "[impossible] %s" msg

type_prohibited_primitive :: Name -> [Expression] -> a
type_prohibited_primitive f es =
  type_prohibited $ printf "the (perhaps unrecognized) primitive function `%s` was not passed the correct number and types of arguments:\n\n  %s" (show f) (show es)

{-
## Processing
-}

executeProgram :: Program -> Executing ()
executeProgram = \case
  Program insts -> do
    executePrelude
    mapM_ executeInstruction insts
    executeMain

executePrelude :: Executing ()
executePrelude = do
  mapM_
    ( \(x, _) ->
        do
          declare variables x
          set variables x undefined -- TODO
    )
    primitive_variables
  mapM_
    ( \(f, params, _) -> do
        declare closures f
        set closures f (fst <$> params, PrimitiveFunctionBody f (fst <$> params))
    )
    primitive_functions

executeMain :: Executing ()
executeMain =
  get' closures mainName >>= \case
    Just ([], _) ->
      void $ executeInstruction (ProcedureCall mainName [])
    Just (_, _) ->
      type_prohibited $ "the `main` function must not take any arguments."
    Nothing ->
      return ()

{-
## Executing
-}

executeInstruction :: Instruction -> Executing (Maybe Value)
executeInstruction = \case
  Block insts -> newScope do
    foldM (\result -> ((result <|>) <$>) . executeInstruction) Nothing insts
  Declaration x _ -> do
    declare variables x
    return Nothing
  Assignment x e -> do
    set variables x =<< evaluateExpression e
    return Nothing
  Function f params _ inst -> do
    declare closures f
    set closures f (fst <$> params, inst)
    return Nothing
  Conditional e inst1 inst2 ->
    evaluateExpression e >>= \case
      Bool True -> newScope $ executeInstruction inst1
      Bool False -> newScope $ executeInstruction inst2
      _ -> type_prohibited $ printf "the condition `%s` must be of type `%s`." (show e) (show BoolType)
  Loop e inst ->
    evaluateExpression e >>= \case
      Bool True -> newScope $ executeInstruction $ Loop e inst
      Bool False -> return Nothing
      _ -> type_prohibited $ printf "the condition `%s` must be of type `%s`." (show e) (show BoolType)
  Return e ->
    Just <$> evaluateExpression e
  ProcedureCall f es -> newScope do
    (xs, inst) <- get closures f
    mapM_
      ( \(x, e) -> do
          declare variables x
          set variables x =<< evaluateExpression e
      )
      (zip xs es)
    void $ executeInstruction inst
    return Nothing
  PrimitiveFunctionBody f xs ->
    executePrimitiveFunctionBody f xs

executePrimitiveFunctionBody :: Name -> [Name] -> Executing (Maybe Value)
executePrimitiveFunctionBody f xs = do
  es <- mapM (get variables) xs
  case (f, es) of
    (Name "&&", [Bool p, Bool q]) ->
      return . Just $ Bool (p && q)
    (Name "||", [Bool p, Bool q]) ->
      return . Just $ Bool (p || q)
    (Name "print_bool", [Bool v]) -> do
      writeOutput (show v)
      return . Just $ Unit
    (Name "print_int", [Int v]) -> do
      writeOutput (show v)
      return . Just $ Unit
    _ -> type_prohibited_primitive f es

{-
## Evaluation
-}

evaluateInstruction :: Instruction -> Executing Value
evaluateInstruction inst =
  executeInstruction inst >>= \case
    Just v -> return v
    Nothing -> type_prohibited $ printf "expected instruction `%s` to return a value." (show inst)

evaluateExpression :: Expression -> Executing Value
evaluateExpression = \case
  Reference x -> get variables x
  Application f es -> newScope do
    (xs, inst) <- get closures f
    mapM_
      ( \(x, e) -> do
          declare variables x
          set variables x =<< evaluateExpression e
      )
      (zip xs es)
    evaluateInstruction inst
  v -> return v

{-
## Scope
-}

newScope :: Executing a -> Executing a
newScope c = do
  scopes %= (emptyScope :) -- push new empty inner scope
  a <- c
  scopes %= tail -- pop inner scope
  return a

declare :: Lens' Scope (Map Name (Maybe v)) -> Name -> Executing ()
declare field k =
  scopes . ix 0 . field . at k .= Just Nothing

set :: Lens' Scope (Map Name (Maybe v)) -> Name -> v -> Executing ()
set field k v = scopes %= go
  where
    go = \case
      [] -> type_prohibited $ printf "the name `%s` cannot not be set before its declaration." (show k)
      scp : scps ->
        case scp ^. field . at k of
          Just _ -> (field . at k .~ Just (Just v)) scp : scps
          Nothing -> scp : go scps

get :: Lens' Scope (Map Name (Maybe v)) -> Name -> Executing v
get field k =
  foldMapBy (<|>) Nothing (^. field . at k) <$> use scopes >>= \case
    Just (Just v) -> return v
    Just Nothing -> throw $ printf "the name `%s` cannot be mentioned before its assignment." (show k)
    Nothing -> type_prohibited $ printf "the name `%s` cannot be mentioned before its declaration." (show k)

get' :: Lens' Scope (Map Name (Maybe v)) -> Name -> Executing (Maybe v)
get' field k =
  foldMapBy (<|>) Nothing (^. field . at k) <$> use scopes >>= \case
    Just (Just v) -> return $ Just v
    Just Nothing -> return Nothing
    Nothing -> return Nothing

writeOutput :: String -> Executing ()
writeOutput s =
  output %= (s :)

readInput :: Executing (Maybe String)
readInput =
  use input >>= \case
    [] -> return Nothing
    s : input' -> do
      input .= input'
      return $ Just s
