module Language.Impe.Executing where

import Control.Applicative
import Control.Lens hiding (Context, set)
import Control.Monad.State as State hiding (get)
import Data.Map as Map
import Language.Impe.Grammar
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
  { _scopes :: [Scope]
  }

data Scope = Scope
  { _closures :: Map Name (Maybe Closure),
    _variables :: Map Name (Maybe Value)
  }

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
    { _scopes = [emptyScope]
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

{-
## Processing
-}

executeProgram :: Program -> Executing ()
executeProgram = \case
  Program inst -> void $ executeInstruction inst

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
      _ -> type_prohibited $ printf "The condition `%s` must be of type `%s`." (show e) (show BoolType)
  Loop e inst ->
    evaluateExpression e >>= \case
      Bool True -> newScope $ executeInstruction $ Loop e inst
      Bool False -> return Nothing
      _ -> type_prohibited $ printf "The condition `%s` must be of type `%s`." (show e) (show BoolType)
  Return e ->
    Just <$> evaluateExpression e

{-
## Evaluation
-}

evaluateExpression :: Expression -> Executing Value
evaluateExpression = \case
  Reference x -> get variables x
  Application f es -> newScope do
    (xs, inst) <- get closures f
    mapM_ (\(x, e) -> set variables x =<< evaluateExpression e) (zip xs es)
    executeInstruction inst >>= \case
      Just e -> return e
      Nothing -> type_prohibited "function body must return."
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
      [] -> type_prohibited $ printf "The name `%s` cannot not be mentioned before its declaration." (show k)
      scp : scps ->
        case scp ^. field . at k of
          Just _ -> (field . at k .~ Just (Just v)) scp : scps
          Nothing -> scp : go scps

get :: Lens' Scope (Map Name (Maybe v)) -> Name -> Executing v
get field k =
  foldMapBy (<|>) Nothing (^. field . at k) <$> use scopes >>= \case
    Just (Just v) -> return v
    Just Nothing -> throw $ printf "The name `%s` is used before its assignment." (show k)
    Nothing -> type_prohibited $ printf "Then name `%s` cannot be mentioned before its declaration." (show k)

-- declareClosure :: Name -> Executing ()
-- declareClosure = undefined

-- setClosure :: Name -> Closure -> Executing ()
-- setClosure = undefined

-- getClosure :: Name -> Executing Closure
-- getClosure = undefined

-- declareVariable :: Name -> Executing ()
-- declareVariable = undefined

-- setVariable :: Name -> Value -> Executing ()
-- setVariable = undefined

-- getVariable :: Name -> Executing Value
-- getVariable = undefined
