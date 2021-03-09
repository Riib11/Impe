module Language.Impe.Executing where

import Control.Applicative
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
# Execution

TODO: description
-}

{-
## Executing Computation
-}

data Context = Context
  { _namespace :: Namespace Name Entry,
    _inputs :: [String],
    _outputs :: [String]
  }

data Entry
  = EntryValue (Maybe Value)
  | EntryClosure (Maybe Closure)

type Execution r a =
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
            . to (map Map.elems)
            . to (map show)
            . to (List.intercalate "\n    "),
        "",
        "  store:",
        List.intercalate "\n"
          . map showEntry
          $ ctx ^. namespace . store . to Map.toList,
        "",
        "inputs:",
        "    " ++ ctx ^. inputs . to (List.intercalate "\n    "),
        "",
        "outputs:",
        "    " ++ ctx ^. outputs . to (List.intercalate "\n    ")
      ]
    where
      showEntry ((x, i), e) = case e of
        EntryValue mb_val -> case mb_val of
          Just val ->
            printf "    variable %s#%s = %s" (show x) (show i) (show val)
          Nothing ->
            printf "    variable %s undefined" (show x)
        EntryClosure mb_clo -> case mb_clo of
          Just ((_, PrimitiveFunctionBody f xs), _) ->
            printf "    function %s(%s) is primitive" (show f) (showArgsNames xs)
          Just ((xs, inst), _) ->
            printf "    function %s(%s) = %s" (show x) (showArgsNames xs) (show inst)
          Nothing ->
            printf "    fun %s undefined" (show x)

{-
### Interface
-}

emptyContext :: Context
emptyContext =
  Context
    { _namespace = mempty,
      _inputs = mempty,
      _outputs = mempty
    }

{-
## Processing
-}

executeProgram :: Program -> Execution r ()
executeProgram = \case
  Program insts -> do
    log Tag_Debug "execute program"
    executePrelude
    mapM_ executeInstruction insts
    executeMain

executePrelude :: Execution r ()
executePrelude = do
  log Tag_Debug "execute prelude"
  mapM_
    ( \(x, _) ->
        do
          declareVariable x
          updateVariable x undefined -- TODO
    )
    primitive_variables
  mapM_
    ( \(f, params, _) -> do
        declareFunction f
        updateFunction f (fst <$> params, PrimitiveFunctionBody f (fst <$> params))
    )
    primitive_functions

executeMain :: Execution r ()
executeMain =
  queryFunction' mainName >>= \case
    Just _ -> do
      log Tag_Debug "execute main"
      void $ executeInstruction (ProcedureCall mainName [])
    Nothing -> return ()

{-
## Execution
-}

executeInstruction :: Instruction -> Execution r (Maybe Value)
executeInstruction inst_ = case inst_ of
  Block insts -> subScope do
    log Tag_Debug "execute block start"
    mb_v <- foldl (<|>) Nothing <$> traverse executeInstruction insts
    log Tag_Debug "execute block end"
    return mb_v
  Declaration x _ -> do
    log Tag_Debug $ printf "execute declaration: %s" (show inst_)
    declareVariable x
    return Nothing
  Assignment x e -> do
    log Tag_Debug $ printf "execute assignment: %s" (show inst_)
    updateVariable x =<< evaluateExpression e
    return Nothing
  Function f params _ inst -> do
    log Tag_Debug $ printf "execute function definition: %s" (show inst_)
    declareFunction f
    updateFunction f (fst <$> params, inst)
    return Nothing
  Conditional e inst1 inst2 -> do
    log Tag_Debug $ printf "execute conditional: %s" (show inst_)
    evaluateExpression e >>= \case
      Bool True -> subScope $ executeInstruction inst1
      Bool False -> subScope $ executeInstruction inst2
      v -> throw $ Excepting.ValueMaltyped e BoolType v
  Loop e inst -> do
    log Tag_Debug $ printf "execute loop: %s" (show inst_)
    evaluateExpression e >>= \case
      Bool True -> subScope $ executeInstruction $ Loop e inst
      Bool False -> return Nothing
      v -> throw $ Excepting.ValueMaltyped e BoolType v
  Return e -> do
    log Tag_Debug $ printf "execute return: %s" (show inst_)
    Just <$> evaluateExpression e
  ProcedureCall f args -> do
    log Tag_Debug $ printf "execute procedure call: %s" (show inst_)
    ((xs, inst), scp) <- queryFunction f
    -- evaluate arguments in outer scope
    vs <- mapM evaluateExpression args
    withScope scp do
      -- declare argument bindings in inner scope
      mapM_
        (\(x, v) -> do declareVariable x; updateVariable x v)
        (zip xs vs)
      void $ executeInstruction inst
      return Nothing
  PrimitiveFunctionBody f xs ->
    executePrimitiveFunctionBody f xs

executePrimitiveFunctionBody :: Name -> [Name] -> Execution r (Maybe Value)
executePrimitiveFunctionBody f xs = do
  log Tag_Debug $
    printf "execute primitive function body: %s" (show $ PrimitiveFunctionBody f xs)
  args <- mapM queryVariable xs
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
      writeOutput (if v then "true" else "false")
      return Nothing
    (Name "output_int", [Int v]) -> do
      writeOutput (show v)
      return Nothing
    _ ->
      throw $ Excepting.UninterpretedPrimitiveFunction f args

{-
## Evaluation
-}

evaluateInstruction :: Instruction -> Execution r Value
evaluateInstruction inst = do
  log Tag_Debug $ printf "evaluate instruction: %s" (show inst)
  executeInstruction inst >>= \case
    Just v -> return v
    Nothing -> throw $ Excepting.InstructionNoReturn inst

evaluateExpression :: Expression -> Execution r Value
evaluateExpression e_ = case e_ of
  Reference x -> do
    log Tag_Debug $ printf "evaluate reference: %s" (show e_)
    queryVariable x
  Application f args -> do
    log Tag_Debug $ printf "evaluate application: %s" (show e_)
    ((xs, inst), scp) <- queryFunction f
    -- evaluate arguments in outer scope
    vs <- mapM evaluateExpression args
    withScope scp do
      -- declare argument bindings in inner scope
      mapM_ (\(x, v) -> do declareVariable x; updateVariable x v) (zip xs vs)
      evaluateInstruction inst
  v -> return v

{-
## Namespace
-}

-- scoping

subScope :: Execution r a -> Execution r a
subScope exe = do
  log Tag_Debug $ "entering local scope"
  modify $ namespace %~ enterScope -- enter local scope
  a <- exe
  log Tag_Debug $ "leaving local scope"
  modify $ namespace %~ leaveScope -- leave local scope
  return a

withScope :: Scope Name -> Execution r a -> Execution r a
withScope scp exe = do
  log Tag_Debug $ printf "entering scope: %s" (show scp)
  scpOri <- gets (^. namespace . scope)
  modify $ namespace . scope .~ scp -- adopt new scope
  a <- exe
  log Tag_Debug $ printf "leaving scope: %s" (show scp)
  modify $ namespace . scope .~ scpOri -- resert original scope
  return a

-- declare

declareVariable :: Name -> Execution r ()
declareVariable x =
  modify $ namespace %~ initialize x (EntryValue Nothing)

declareFunction :: Name -> Execution r ()
declareFunction f =
  modify $ namespace %~ initialize f (EntryClosure Nothing)

-- update

updateVariable :: Name -> Value -> Execution r ()
updateVariable x val =
  gets (^. namespace . at x) >>= \case
    Just (EntryValue _) ->
      modify $ namespace . at x .~ Just ((EntryValue . Just) val)
    Just (EntryClosure _) ->
      throw $ Excepting.VariableNo x
    Nothing ->
      modify $ namespace . at x .~ Just ((EntryValue . Just) val)

updateFunction :: Name -> Binding -> Execution r ()
updateFunction f bnd =
  gets (^. namespace . at f) >>= \case
    Just (EntryClosure _) -> do
      scp <- gets (^. namespace . scope)
      modify $ namespace . at f .~ Just ((EntryClosure . Just) (bnd, scp))
    Just (EntryValue _) ->
      throw $ Excepting.FunctionNo f
    Nothing -> do
      scp <- gets (^. namespace . scope)
      modify $ namespace . at f .~ Just ((EntryClosure . Just) (bnd, scp))

-- query

queryVariable :: Name -> Execution r Value
queryVariable x =
  gets (^. namespace . at x) >>= \case
    Just (EntryValue (Just val)) ->
      return val
    Just (EntryValue Nothing) ->
      throw $ Excepting.VariableUninitializedMention x
    Just (EntryClosure _) ->
      throw $ Excepting.VariableNo x
    Nothing ->
      throw $ Excepting.VariableUndeclaredMention x

queryVariable' :: Name -> Execution r (Maybe (Maybe Value))
queryVariable' x =
  gets (^. namespace . at x) >>= \case
    Just (EntryValue (Just val)) ->
      return $ Just (Just val)
    Just (EntryValue Nothing) ->
      return $ Just Nothing
    Just (EntryClosure _) ->
      throw $ Excepting.VariableNo x
    Nothing ->
      return Nothing

queryFunction :: Name -> Execution r Closure
queryFunction f =
  gets (^. namespace . at f) >>= \case
    Just (EntryClosure (Just clo)) -> return clo
    Just (EntryClosure Nothing) -> throw $ Excepting.FunctionUninitializedMention f
    Just (EntryValue _) -> throw $ Excepting.FunctionNo f
    Nothing -> throw $ Excepting.FunctionUninitializedMention f

queryFunction' :: Name -> Execution r (Maybe (Maybe Closure))
queryFunction' f =
  gets (^. namespace . at f) >>= \case
    Just (EntryClosure (Just clo)) -> return $ Just (Just clo)
    Just (EntryClosure Nothing) -> return $ Just Nothing
    Just (EntryValue _) -> throw $ Excepting.FunctionNo f
    Nothing -> return Nothing

-- I/O

writeOutput :: String -> Execution r ()
writeOutput s =
  modify $ outputs %~ (s :)

readInput :: Execution r (Maybe String)
readInput =
  gets (^. inputs) >>= \case
    [] -> return Nothing
    s : inputs' -> do
      modify $ inputs .~ inputs'
      return $ Just s

logOutputs ::
  ( Member (Output Log) r,
    Member (State Context) r
  ) =>
  Sem r ()
logOutputs =
  gets (^. outputs . to reverse) >>= \case
    [] -> return ()
    os -> log Tag_Output $ List.intercalate "\n" os

resetOutputs ::
  ( Member (Output Log) r,
    Member (State Context) r
  ) =>
  Sem r ()
resetOutputs =
  modify $ outputs .~ mempty

{-
## Excepting
-}

throw ::
  Member (Error Excepting.Exception) r =>
  Excepting.Executing ->
  Sem r a
throw = Excepting.throw . Excepting.Executing
