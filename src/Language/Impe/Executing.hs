module Language.Impe.Executing where

import Control.Applicative
import Control.Lens
import Control.Monad
import Data.Map as Map hiding (foldr, map)
import Language.Impe.Grammar
import Language.Impe.Primitive
import Polysemy
import Polysemy.Error as Error
import Polysemy.Output as Output
import Polysemy.State as State
import Text.Printf

{-
# Execution

TODO: description
-}

{-
## Executing Computation
-}

data ExecutionContext = ExecutionContext
  { _scopes :: [Scope],
    _variables :: Map UID (Maybe Value),
    _functions :: Map UID (Maybe Closure),
    _indexUID :: IndexUID,
    _inputs :: [String],
    _outputs :: [String]
  }

data Scope = Scope
  { _variableUIDs :: Map Name UID,
    _functionUIDs :: Map Name UID
  }

type UID = (Name, IndexUID)

type IndexUID = Int

type Closure = ([Name], [Scope], Instruction)

type Value = Expression

data ExecutionError = ExecutionError String ExecutionContext

type ExecutionLog = String

type Execution a = Sem '[State ExecutionContext, Error ExecutionError, Output ExecutionLog] a

makeLenses ''Scope
makeLenses ''ExecutionContext

-- instances

instance Show ExecutionContext where
  show ctx =
    unlines
      [ "execution context:",
        -- TODO: can't print scopes since are recursively nested?
        -- "  scopes:",
        -- unlines $ show <$> (zip (reverse $ ctx ^. scopes) [(0 :: Int) ..]),
        "  variables UIDs:",
        unlines
          . map
            ( \((x, i), mb_v) -> case mb_v of
                Just v -> printf "    %s#%s = %s" (show x) (show i) (show v)
                Nothing -> printf "    %s#%s undefined" (show x) (show i)
            )
          . toList
          $ ctx ^. variables,
        "  functions UIDs:",
        unlines
          . map
            ( \((f, i), mb_clo) -> case mb_clo of
                -- TODO: can't print closure since is recursively nested?
                Just _ -> printf "    %s#%s = ..." (show f) (show i)
                Nothing -> printf "    %s#%s undefined" (show f) (show i)
            )
          . toList
          $ ctx ^. functions,
        "  inputs:",
        "    " ++ show (ctx ^. inputs),
        "  outputs:",
        "    " ++ show (ctx ^. outputs)
      ]

instance Show Scope where
  show scp =
    unlines
      [ "    [" ++ show scp ++ "]:",
        "      variables:",
        unlines . map (\(x, (_, i)) -> printf "        %s#%s" (show x) (show i)) . toList $ scp ^. variableUIDs,
        "      functions:",
        unlines . map (\(f, (_, i)) -> printf "        %s#%s" (show f) (show i)) . toList $ scp ^. functionUIDs
      ]

instance Show ExecutionError where
  show = \case
    ExecutionError msg ctx -> printf "%s\n\n%s" msg (show ctx)

{-
### Interface
-}

runExecution ::
  Execution a ->
  ( [ExecutionLog],
    Either ExecutionError (ExecutionContext, a)
  )
runExecution =
  run
    . runOutputList
    . runError
    . runState emptyExecutionContext

execExecution ::
  Execution a ->
  ( [ExecutionLog],
    Either ExecutionError ExecutionContext
  )
execExecution =
  run
    . runOutputList
    . runError
    . execState emptyExecutionContext

emptyExecutionContext :: ExecutionContext
emptyExecutionContext =
  ExecutionContext
    { _scopes = [emptyScope],
      _indexUID = indexUID_init,
      _variables = Map.empty,
      _functions = Map.empty,
      _inputs = [],
      _outputs = []
    }

emptyScope :: Scope
emptyScope =
  Scope
    { _functionUIDs = Map.empty,
      _variableUIDs = Map.empty
    }

indexUID_init :: IndexUID
indexUID_init = 0

indexUID_next :: IndexUID -> IndexUID
indexUID_next = (1 +)

execution_error :: String -> Execution a
execution_error msg = do
  ctx <- State.get
  throw $ ExecutionError msg ctx

type_prohibited :: String -> Execution a
type_prohibited msg = execution_error $ printf "[type-prohibited] %s" msg

impossible :: String -> Execution a
impossible msg = execution_error $ printf "[impossible] %s" msg

type_prohibited_primitive :: Name -> [Expression] -> Execution a
type_prohibited_primitive f es =
  type_prohibited $ printf "the (perhaps unrecognized) primitive function `%s` was not passed the correct number and types of arguments:\n\n  %s" (show f) (show es)

{-
## Processing
-}

executeProgram :: Program -> Execution ()
executeProgram = \case
  Program insts -> do
    executePrelude
    mapM_ executeInstruction insts
    executeMain

executePrelude :: Execution ()
executePrelude = do
  mapM_
    ( \(x, _) ->
        do
          declareVariable x
          setVariable x undefined -- TODO
    )
    primitive_variables
  mapM_
    ( \(f, params, _) -> do
        declareFunction f
        setFunction f (fst <$> params, PrimitiveFunctionBody f (fst <$> params))
    )
    primitive_functions

executeMain :: Execution ()
executeMain =
  getFunction' mainName >>= \case
    Just ([], scps, _) ->
      enterScopes scps . void $
        executeInstruction (ProcedureCall mainName [])
    Just (_, _, _) ->
      type_prohibited $ "the `main` function must not take any arguments."
    Nothing ->
      return ()

{-
## Execution
-}

executeInstruction :: Instruction -> Execution (Maybe Value)
executeInstruction = \case
  Block insts -> newScope do
    foldM (\result -> ((result <|>) <$>) . executeInstruction) Nothing insts
  Declaration x _ -> do
    declareVariable x
    return Nothing
  Assignment x e -> do
    setVariable x =<< evaluateExpression e
    return Nothing
  Function f params _ inst -> do
    declareFunction f
    setFunction f (fst <$> params, inst)
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
  ProcedureCall f args -> do
    (xs, scps, inst) <- getFunction f
    enterScopes scps do
      mapM_
        ( \(x, e) -> do
            v <- evaluateExpression e
            declareVariable x
            setVariable x v
        )
        (zip xs args)
      void $ executeInstruction inst
      return Nothing
  PrimitiveFunctionBody f xs ->
    executePrimitiveFunctionBody f xs

executePrimitiveFunctionBody :: Name -> [Name] -> Execution (Maybe Value)
executePrimitiveFunctionBody f xs = do
  args <- mapM getVariable xs
  case (f, args) of
    (Name "&&", [Bool p, Bool q]) ->
      return . Just $ Bool (p && q)
    (Name "||", [Bool p, Bool q]) ->
      return . Just $ Bool (p || q)
    (Name "+", [Int x, Int y]) ->
      return . Just $ Int (x + y)
    (Name "-", [Int x, Int y]) ->
      return . Just $ Int (x - y)
    (Name "*", [Int x, Int y]) ->
      return . Just $ Int (x * y)
    (Name "outputs_bool", [Bool v]) -> do
      writeOutput (show v)
      return . Just $ Unit
    (Name "outputs_int", [Int v]) -> do
      writeOutput (show v)
      return . Just $ Unit
    _ -> type_prohibited_primitive f args

{-
## Evaluation
-}

evaluateInstruction :: Instruction -> Execution Value
evaluateInstruction inst =
  executeInstruction inst >>= \case
    Just v -> return v
    Nothing -> type_prohibited $ printf "expected instruction `%s` to return a value." (show inst)

evaluateExpression :: Expression -> Execution Value
evaluateExpression = \case
  Reference x -> getVariable x
  Application f es -> do
    (xs, scps, inst) <- getFunction f
    enterScopes scps do
      mapM_
        ( \(x, e) -> do
            v <- evaluateExpression e
            declareVariable x
            setVariable x v
        )
        (zip xs es)
      evaluateInstruction inst
  v -> return v

{-
## Scope
-}

newScope :: Execution a -> Execution a
newScope = enterScopes [emptyScope]

enterScopes :: [Scope] -> Execution a -> Execution a
enterScopes scps c = do
  scpsOriginal <- gets (^. scopes)
  modify $ scopes %~ (scps ++) -- enter new scopes
  a <- c
  modify $ scopes .~ scpsOriginal -- reset to original scopes
  return a

-- UID

newIndexUID :: Execution IndexUID
newIndexUID = do
  i <- gets (^. indexUID)
  modify $ indexUID %~ indexUID_next
  return i

newUID :: Name -> Execution UID
newUID x = do
  i <- newIndexUID
  return (x, i)

-- declare

declareVariable :: Name -> Execution ()
declareVariable x = do
  uid <- newUID x
  modify $ scopes . ix 0 . variableUIDs . at x .~ Just uid
  modify $ variables . at uid .~ Just Nothing

declareFunction :: Name -> Execution ()
declareFunction f = do
  uid <- newUID f
  modify $ scopes . ix 0 . functionUIDs . at f .~ Just uid
  modify $ functions . at uid .~ Just Nothing

-- set

setVariable :: Name -> Value -> Execution ()
setVariable x v = go =<< gets (^. scopes)
  where
    go :: [Scope] -> Execution ()
    go = \case
      [] -> type_prohibited $ printf "the variable `%s` cannot be set before its declaration." (show x)
      scp : scps ->
        case scp ^. variableUIDs . at x of
          Just uid -> modify $ variables . ix uid .~ Just v
          Nothing -> go scps

setFunction :: Name -> ([Name], Instruction) -> Execution ()
setFunction f (xs, inst) = go =<< gets (^. scopes)
  where
    go = \case
      [] -> type_prohibited $ printf "the function `%s` cannot be set before its declaration." (show f)
      scp : scps ->
        case scp ^. functionUIDs . at f of
          Just uid -> do
            scpsLocal <- gets (^. scopes) -- capture local scopes
            modify $ functions . ix uid .~ Just (xs, scpsLocal, inst)
          Nothing -> go scps

-- get

getVariable :: Name -> Execution Value
getVariable x =
  foldr (<|>) Nothing . map (^. variableUIDs . at x) <$> gets (^. scopes) >>= \case
    Just uid ->
      gets (^. variables . at uid) >>= \case
        Just (Just v) -> return v
        _ -> execution_error $ printf "the variable `%s` cannot be mentioned before its assignment." (show x)
    Nothing -> type_prohibited $ printf "the variable `%s` cannot be mentioned before its declaration." (show x)

getVariable' :: Name -> Execution (Maybe Value)
getVariable' x =
  foldr (<|>) Nothing . map (^. variableUIDs . at x) <$> gets (^. scopes) >>= \case
    Just uid ->
      gets (^. variables . at uid) >>= \case
        Just (Just v) -> return $ Just v
        _ -> return Nothing
    Nothing -> return Nothing

getFunction :: Name -> Execution Closure
getFunction f =
  foldr (<|>) Nothing . map (^. functionUIDs . at f) <$> gets (^. scopes) >>= \case
    Just uid ->
      gets (^. functions . at uid) >>= \case
        Just (Just clo) -> return clo
        _ -> execution_error $ printf "the function `%s` cannot be mentioned before its assignment." (show f)
    Nothing -> type_prohibited $ printf "the function `%s` cannot be mentioned before its declaration." (show f)

getFunction' :: Name -> Execution (Maybe Closure)
getFunction' f =
  foldr (<|>) Nothing . map (^. functionUIDs . at f) <$> gets (^. scopes) >>= \case
    Just uid ->
      gets (^. functions . at uid) >>= \case
        Just (Just clo) -> return $ Just clo
        _ -> return Nothing
    Nothing -> return Nothing

-- I/O

writeOutput :: String -> Execution ()
writeOutput s =
  modify $ outputs %~ (s :)

readInput :: Execution (Maybe String)
readInput =
  gets (^. inputs) >>= \case
    [] -> return Nothing
    s : inputs' -> do
      modify $ inputs .~ inputs'
      return $ Just s
