module Language.Impe.Executing where

import Control.Applicative
import Control.Lens hiding (Context, set)
import Control.Monad.State hiding (get)
import qualified Control.Monad.State as State (get)
import Data.Map as Map hiding (foldr, map)
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
    _variables :: Map UID (Maybe Value),
    _functions :: Map UID (Maybe Closure),
    _indexUID :: IndexUID,
    _input :: [String],
    _output :: [String]
  }

data Scope = Scope
  { _variableUIDs :: Map Name UID,
    _functionUIDs :: Map Name UID
  }

type UID = (Name, IndexUID)

type IndexUID = Int

type Closure = ([Name], [Scope], Instruction)

type Value = Expression

data Error = Error String Context

makeLenses ''Scope
makeLenses ''Context

instance Show Context where
  show ctx =
    unlines
      [ "executing context:",
        -- can't print scopes since are recursively nested
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
                -- can't print closure since is recursively nested
                Just _ -> printf "    %s#%s = ..." (show f) (show i)
                Nothing -> printf "    %s#%s undefined" (show f) (show i)
            )
          . toList
          $ ctx ^. functions,
        "  input:",
        "    " ++ show (ctx ^. input),
        "  output:",
        "    " ++ show (ctx ^. output)
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

{-
### Interface
-}

runExecuting :: Executing a -> Either Error (a, Context)
runExecuting c = runStateT c emptyContext

emptyContext :: Context
emptyContext =
  Context
    { _scopes = [emptyScope],
      _indexUID = indexUID_init,
      _variables = Map.empty,
      _functions = Map.empty,
      _input = [],
      _output = []
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

execution_error :: String -> Executing a
execution_error msg = do
  ctx <- State.get
  lift . Left $ Error msg ctx

type_prohibited :: String -> Executing a
type_prohibited msg = execution_error $ printf "[type-prohibited] %s" msg

impossible :: String -> Executing a
impossible msg = execution_error $ printf "[impossible] %s" msg

type_prohibited_primitive :: Name -> [Expression] -> Executing a
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

executeMain :: Executing ()
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
## Executing
-}

executeInstruction :: Instruction -> Executing (Maybe Value)
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

executePrimitiveFunctionBody :: Name -> [Name] -> Executing (Maybe Value)
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
    (Name "output_bool", [Bool v]) -> do
      writeOutput (show v)
      return . Just $ Unit
    (Name "output_int", [Int v]) -> do
      writeOutput (show v)
      return . Just $ Unit
    _ -> type_prohibited_primitive f args

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

newScope :: Executing a -> Executing a
newScope = enterScopes [emptyScope]

enterScopes :: [Scope] -> Executing a -> Executing a
enterScopes scps c = do
  scpsOriginal <- use scopes
  scopes %= (scps ++) -- enter new scopes
  a <- c
  scopes .= scpsOriginal -- reset to original scopes
  return a

-- UID

newIndexUID :: Executing IndexUID
newIndexUID = do
  i <- use indexUID
  indexUID %= indexUID_next
  return i

newUID :: Name -> Executing UID
newUID x = do
  i <- newIndexUID
  return (x, i)

-- declare

declareVariable :: Name -> Executing ()
declareVariable x = do
  uid <- newUID x
  scopes . ix 0 . variableUIDs . at x .= Just uid
  variables . at uid .= Just Nothing

declareFunction :: Name -> Executing ()
declareFunction f = do
  uid <- newUID f
  scopes . ix 0 . functionUIDs . at f .= Just uid
  functions . at uid .= Just Nothing

-- set

setVariable :: Name -> Value -> Executing ()
setVariable x v = go =<< use scopes
  where
    go :: [Scope] -> Executing ()
    go = \case
      [] -> type_prohibited $ printf "the variable `%s` cannot be set before its declaration." (show x)
      scp : scps ->
        case scp ^. variableUIDs . at x of
          Just uid -> variables . ix uid .= Just v
          Nothing -> go scps

setFunction :: Name -> ([Name], Instruction) -> Executing ()
setFunction f (xs, inst) = go =<< use scopes
  where
    go = \case
      [] -> type_prohibited $ printf "the function `%s` cannot be set before its declaration." (show f)
      scp : scps ->
        case scp ^. functionUIDs . at f of
          Just uid -> do
            scpsLocal <- use scopes -- capture local scopes
            functions . ix uid .= Just (xs, scpsLocal, inst)
          Nothing -> go scps

-- get

getVariable :: Name -> Executing Value
getVariable x =
  foldr (<|>) Nothing . map (^. variableUIDs . at x) <$> use scopes >>= \case
    Just uid ->
      use (variables . at uid) >>= \case
        Just (Just v) -> return v
        _ -> execution_error $ printf "the variable `%s` cannot be mentioned before its assignment." (show x)
    Nothing -> type_prohibited $ printf "the variable `%s` cannot be mentioned before its declaration." (show x)

getVariable' :: Name -> Executing (Maybe Value)
getVariable' x =
  foldr (<|>) Nothing . map (^. variableUIDs . at x) <$> use scopes >>= \case
    Just uid ->
      use (variables . at uid) >>= \case
        Just (Just v) -> return $ Just v
        _ -> return Nothing
    Nothing -> return Nothing

getFunction :: Name -> Executing Closure
getFunction f =
  foldr (<|>) Nothing . map (^. functionUIDs . at f) <$> use scopes >>= \case
    Just uid ->
      use (functions . at uid) >>= \case
        Just (Just clo) -> return clo
        _ -> execution_error $ printf "the function `%s` cannot be mentioned before its assignment." (show f)
    Nothing -> type_prohibited $ printf "the function `%s` cannot be mentioned before its declaration." (show f)

getFunction' :: Name -> Executing (Maybe Closure)
getFunction' f =
  foldr (<|>) Nothing . map (^. functionUIDs . at f) <$> use scopes >>= \case
    Just uid ->
      use (functions . at uid) >>= \case
        Just (Just clo) -> return $ Just clo
        _ -> return Nothing
    Nothing -> return Nothing

-- I/O

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
