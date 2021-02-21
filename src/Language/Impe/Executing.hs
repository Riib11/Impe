module Language.Impe.Executing where

import Control.Applicative
import Control.Lens hiding (Context, locally)
import Control.Monad.State as State
import Data.Map as Map
import Data.Maybe as Maybe
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
  { _functions :: Map Name ([Name], Instruction),
    _variables :: Map Name Expression
  }

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
    { _functions = Map.empty,
      _variables = Map.empty
    }

throw :: Error -> Executing a
throw = lift . Left

{-
## Processing
-}

processStatement :: Statement -> Executing ()
processStatement = \case
  Function f params _ inst -> do
    setFunction f (fst <$> params, inst)

{-
## Evaluating
-}

evaluateProgram :: Program -> Executing Expression
evaluateProgram = \case
  Program stmts inst -> do
    mapM_ processStatement stmts
    fromJust <$> executeInstruction inst

{-
## Executing
-}

executeInstruction :: Instruction -> Executing (Maybe Expression)
executeInstruction = \case
  Block insts -> locally do
    foldM (\mbe inst -> (<|>) mbe <$> executeInstruction inst) Nothing insts
  Declaration _ _ ->
    return Nothing
  Assignment x e -> do
    setVariable x =<< evaluateExpression e
    return Nothing
  Conditional e inst1 inst2 ->
    evaluateExpression e >>= \case
      Bool True -> executeInstruction inst1
      Bool False -> executeInstruction inst2
      _ -> error $ printf "[type-enforced] conditional of non-Bool expression\n\n  %s\n\n" (show e)
  Loop e inst ->
    evaluateExpression e >>= \case
      Bool True -> executeInstruction $ Loop e inst
      Bool False -> return Nothing
      _ -> error $ printf "[type-enforced] loop of non-Bool expression\n\n  %s\n\n" (show e)
  Return e ->
    Just <$> evaluateExpression e

{-
## Evaluation
-}

evaluateExpression :: Expression -> Executing Expression
evaluateExpression = \case
  Reference x -> getVariable x
  Application f es -> locally do
    (xs, inst) <- getFunction f
    mapM_ (\(x, e) -> setVariable x =<< evaluateExpression e) (zip xs es)
    fromJust <$> executeInstruction inst
  v -> return v

{-
## Utilities
-}

getFunction :: Name -> Executing ([Name], Instruction)
getFunction f =
  use (functions . at f) >>= \case
    Just impl -> return impl
    Nothing -> throw $ printf "the function\n\n  %s\n\n is not declared before its mention." (show f)

setFunction :: Name -> ([Name], Instruction) -> Executing ()
setFunction f impl =
  functions . at f .= Just impl -- shadowing

getVariable :: Name -> Executing Expression
getVariable x =
  use (variables . at x) >>= \case
    Just e -> return e
    Nothing -> throw $ printf "the variable\n\n  %s\n\nis not declared before its mention." (show x)

setVariable :: Name -> Expression -> Executing ()
setVariable x e =
  variables . at x .= Just e -- shadowing

locally :: Executing a -> Executing a
locally c = do
  st <- get
  (a, _) <- lift $ runStateT c st
  return a
