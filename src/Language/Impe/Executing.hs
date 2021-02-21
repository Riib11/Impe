module Language.Impe.Executing where

import Control.Applicative
import Control.Lens hiding (Context, locally)
import Control.Monad.State as State
-- import Data.List as List hiding (delete, insert)
-- import Data.Map as Map
-- import Data.Maybe as Maybe
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
  { _closures :: Store Name Closure,
    _variables :: Store Name Value
  }

type Store k v = [(k, v)]

type Closure = ([Name], Instruction)

type Value = Expression

type Error = String

makeLenses ''Context

{-
### Interface
-}

runExecuting :: Executing a -> Either Error (a, Context)
runExecuting c = runStateT c emptyContext

emptyContext :: Context
emptyContext =
  Context
    { _closures = [],
      _variables = []
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
  Declaration _ _ ->
    return Nothing
  Assignment x e -> do
    setVariable x =<< evaluateExpression e
    return Nothing
  Function f params _ inst -> do
    setClosure f (fst <$> params, inst)
    return Nothing
  Conditional e inst1 inst2 ->
    evaluateExpression e >>= \case
      Bool True -> newScope $ executeInstruction inst1
      Bool False -> newScope $ executeInstruction inst2
      _ -> type_prohibited "conditional condition must be of type `Bool`."
  Loop e inst ->
    evaluateExpression e >>= \case
      Bool True -> newScope $ executeInstruction $ Loop e inst
      Bool False -> return Nothing
      _ -> type_prohibited "loop condition must be of type `Bool`."
  Return e ->
    Just <$> evaluateExpression e

{-
## Evaluation
-}

evaluateExpression :: Expression -> Executing Value
evaluateExpression = \case
  Reference x -> getVariable x
  Application f es -> newScope do
    (xs, inst) <- getClosure f
    mapM_ (\(x, e) -> setVariable x =<< evaluateExpression e) (zip xs es)
    executeInstruction inst >>= \case
      Just e -> return e
      Nothing -> type_prohibited "function body must return."
  v -> return v

{-
## Scope
-}

newScope :: Executing a -> Executing a
newScope c = do
  vars <- use variables -- capture original variables
  clos <- use closures -- capture original closures
  a <- c -- do scoped computation
  resetVariables vars -- reset shadowed variables
  resetClosures clos -- reset shadowed closures
  return a -- result of scoped computation

resetVariables :: Store Name Value -> Executing ()
resetVariables varsOld = go =<< use variables
  where
    go :: Store Name Value -> Executing ()
    go vars =
      if all (uncurry (==)) $ zip (keys vars) (keys varsOld)
        then -- base
          return ()
        else -- induction
        case vars of
          (x, _) : vars' -> do
            case lookup x varsOld of -- shadowed by new scope?
              Just v -> setVariable x v -- then reset to old value
              Nothing -> deleteVariable x -- else delete from scope
            go vars'
          _ -> impossible "A new scope could not have deleted variables."

resetClosures :: [(Name, Closure)] -> Executing ()
resetClosures closOld = go =<< use closures
  where
    go clos =
      if all (uncurry (==)) $ zip (keys clos) (keys closOld)
        then -- base
          return ()
        else -- induction
        case clos of
          (f, _) : clos' -> do
            case lookup f closOld of -- shadowed by new scope?
              Just clo -> setClosure f clo -- then reset to old closure
              Nothing -> deleteClosure f -- else delete from scope
            go clos'
          _ -> impossible "A new scope could not have deleted closures."

{-
## Store
-}

getClosure :: Name -> Executing Closure
getClosure f =
  closures `uses` lookup f >>= \case
    Just clo -> return clo
    Nothing -> throw $ printf "the function\n\n  %s\n\n is not declared before its mention." (show f)

setClosure :: Name -> Closure -> Executing ()
setClosure f clo = closures `modifying` insert f clo

deleteClosure :: Name -> Executing ()
deleteClosure f = closures `modifying` delete f

getVariable :: Name -> Executing Value
getVariable x =
  variables `uses` lookup x >>= \case
    Just v -> return v
    Nothing -> throw $ printf "the variable\n\n  %s\n\nis not declared before its mention." (show x)

setVariable :: Name -> Value -> Executing ()
setVariable x v = variables `modifying` insert x v

deleteVariable :: Name -> Executing ()
deleteVariable x = variables `modifying` delete x

{-
## Store
-}

keys :: Store k v -> [k]
keys = map fst

values :: Store k v -> [v]
values = map snd

-- at key insert value in store
-- if key exists, then overwrite old value
-- otherwise, prepend new entry to store
insert :: Eq k => k -> v -> Store k v -> Store k v
insert k v sOrig = go sOrig
  where
    go = \case
      [] -> (k, v) : sOrig
      (k', v') : s' ->
        if k == k'
          then (k, v) : s'
          else (k', v') : go s'

-- at key delete entry in store
-- only first entry with key is deleted
delete :: Eq k => k -> Store k v -> Store k v
delete k = \case
  [] -> []
  (k', v') : s ->
    if k == k'
      then s
      else (k', v') : delete k s
