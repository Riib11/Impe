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
  | EntryPrimitiveFunction

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
      showEntry (uid, e) = case e of
        EntryValue mb_val -> case mb_val of
          Just val ->
            printf "    variable %s = %s" (show uid) (show val)
          Nothing ->
            printf "    variable %s uninitialized" (show uid)
        EntryClosure mb_clo -> case mb_clo of
          Just ((xs, inst), _) ->
            printf "    function %s(%s) = %s" (show uid) (showArgsNames xs) (show inst)
          Nothing ->
            printf "    function %s uninitialized" (show uid)
        EntryPrimitiveFunction ->
          printf "    function %s primitive" (show uid)

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
  -- primitive variables
  mapM_
    ( \(x, _, e) ->
        do
          declareVariable x
          adjustVariable x =<< evaluateExpression e
    )
    primitive_variables
  -- primitive functions
  mapM_
    (\(f, _, _) -> declarePrimitiveFunction f)
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
  Block insts -> withLocalScope do
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
    adjustVariable x =<< evaluateExpression e
    return Nothing
  Function f params _ inst -> do
    log Tag_Debug $ printf "execute function definition: %s" (show inst_)
    declareFunction f
    adjustFunction f (fst <$> params, inst)
    return Nothing
  Conditional e inst1 inst2 -> do
    log Tag_Debug $ printf "execute conditional: %s" (show inst_)
    evaluateExpression e >>= \case
      Bool True -> withLocalScope $ executeInstruction inst1
      Bool False -> withLocalScope $ executeInstruction inst2
      v -> throw $ Excepting.ValueMaltyped e BoolType v
  Loop e inst -> do
    log Tag_Debug $ printf "execute loop: %s" (show inst_)
    evaluateExpression e >>= \case
      Bool True -> do
        log Tag_Debug $ printf "evaluate loop condition to true: %s" (show e)
        executeInstruction inst >>= \case
          Just v -> do
            log Tag_Debug $ printf "execute loop iteration to return value: %s" (show v)
            return $ Just v
          Nothing ->
            withLocalScope $ executeInstruction $ Loop e inst
      Bool False -> do
        log Tag_Debug $ printf "evaluate loop condition to false: %s" (show e)
        return Nothing
      v -> throw $ Excepting.ValueMaltyped e BoolType v
  Return e -> do
    log Tag_Debug $ printf "execute return: %s" (show inst_)
    Just <$> evaluateExpression e
  ProcedureCall f args -> do
    log Tag_Debug $ printf "execute procedure call: %s" (show inst_)
    queryFunction f >>= \case
      -- closure
      Left ((xs, inst), scp) -> withLocalScope do
        -- evaluate arguments in local scope
        vs <- mapM evaluateExpression args
        -- init param vars in local scope (will be GC'ed by `withLocalScope`)
        mapM_ (uncurry initializeVariable) (zip xs vs)
        withScope scp do
          -- execute instruction in function scope
          void $ executeInstruction inst
          -- ignore result
          return Nothing
      -- primitive function
      Right pf -> do
        -- evaluate arguments in outer scope
        args' <- mapM evaluateExpression args
        -- hand-off to execute primitive function
        executePrimitiveFunction pf args'

executePrimitiveFunction :: Name -> [Expression] -> Execution r (Maybe Value)
executePrimitiveFunction f args = do
  log Tag_Debug $ printf "execute primitive function: %s(%s)" (show f) (showArgs args)
  case (f, args) of
    -- bool
    (Name "&&", [Bool p, Bool q]) -> return . Just $ Bool (p && q)
    (Name "||", [Bool p, Bool q]) -> return . Just $ Bool (p || q)
    (Name "output_bool", [Bool v]) -> do
      writeOutput (if v then "true" else "false")
      return Nothing
    -- int
    (Name "+", [Int x, Int y]) -> return . Just $ Int (x + y)
    (Name "-", [Int x, Int y]) -> return . Just $ Int (x - y)
    (Name "*", [Int x, Int y]) -> return . Just $ Int (x * y)
    (Name "^", [Int x, Int y]) -> return . Just $ Int (x ^ y)
    (Name "=", [Int x, Int y]) -> return . Just $ Bool (x == y)
    (Name ">", [Int x, Int y]) -> return . Just $ Bool (x > y)
    (Name ">=", [Int x, Int y]) -> return . Just $ Bool (x >= y)
    (Name "<", [Int x, Int y]) -> return . Just $ Bool (x < y)
    (Name "<=", [Int x, Int y]) -> return . Just $ Bool (x <= y)
    (Name "output_int", [Int v]) -> writeOutput (show v) >> return Nothing
    -- string
    (Name "<>", [String a, String b]) -> return . Just $ String (a <> b)
    (Name "output_string", [String a]) -> writeOutput a >> return Nothing
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
    queryFunction f >>= \case
      -- constructed function
      Left ((xs, inst), scp) -> withLocalScope do
        -- evaluate arguments in local scope
        vs <- mapM evaluateExpression args
        -- init param vars in local scope (will be GC'ed by `withLocalScope`)
        mapM_ (uncurry initializeVariable) (zip xs vs)
        withScope scp do
          -- declare argument bindings in inner scope
          mapM_ (uncurry adjustVariable) (zip xs vs)
          -- evaluate instruction, returning result
          evaluateInstruction inst
      -- primitive function
      Right pf -> do
        -- evaluate arguments in outer scope
        args' <- mapM evaluateExpression args
        -- hand-off to execute primitive function
        executePrimitiveFunction pf args' >>= \case
          Just v -> return v
          Nothing -> throw $ Excepting.ExpressionNoValue e_
  v -> return v

{-
## Namespace
-}

-- scoping

withLocalScope :: Execution r a -> Execution r a
withLocalScope exe = do
  log Tag_Debug $ "entering local scope"
  modify $ namespace %~ enterLocalScope -- enter local scope
  a <- exe
  log Tag_Debug $ "leaving local scope"
  modify $ namespace %~ leaveLocalScope -- leave local scope
  return a

withScope :: Scope Name -> Execution r a -> Execution r a
withScope scp exe = do
  log Tag_Debug $ printf "entering scope: %s" (show scp)
  scpOri <- gets (^. namespace . scope) -- capture original scope
  modify $ namespace . scope .~ scp -- adopt new scope
  a <- exe
  log Tag_Debug $ printf "leaving scope: %s" (show scp)
  modify $ namespace . scope .~ scpOri -- reset to original scope
  return a

-- declare

declareVariable :: Name -> Execution r ()
declareVariable x =
  modify $ namespace %~ initialize x (EntryValue Nothing)

declareFunction :: Name -> Execution r ()
declareFunction f =
  modify $ namespace %~ initialize f (EntryClosure Nothing)

declarePrimitiveFunction :: Name -> Execution r ()
declarePrimitiveFunction f =
  modify $ namespace %~ initialize f EntryPrimitiveFunction

-- update

adjustVariable :: Name -> Value -> Execution r ()
adjustVariable x val =
  gets (^. namespace . at x) >>= \case
    Just (EntryValue _) ->
      modify $ namespace . at x .~ Just ((EntryValue . Just) val)
    Just (EntryClosure _) ->
      throw $ Excepting.VariableNo x
    Just EntryPrimitiveFunction ->
      throw $ Excepting.VariableNo x
    Nothing ->
      modify $ namespace . at x .~ Just ((EntryValue . Just) val)

adjustFunction :: Name -> Binding -> Execution r ()
adjustFunction f bnd =
  gets (^. namespace . at f) >>= \case
    Just (EntryClosure _) -> do
      scp <- gets (^. namespace . scope)
      modify $ namespace . at f .~ Just ((EntryClosure . Just) (bnd, scp))
    Just (EntryValue _) ->
      throw $ Excepting.FunctionNo f
    Just EntryPrimitiveFunction ->
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
    Just EntryPrimitiveFunction ->
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
    Just EntryPrimitiveFunction ->
      throw $ Excepting.VariableNo x
    Nothing ->
      return Nothing

-- closure or primitive function name
queryFunction :: Name -> Execution r (Either Closure Name)
queryFunction f =
  gets (^. namespace . at f) >>= \case
    Just (EntryClosure (Just clo)) -> return $ Left clo
    Just (EntryClosure Nothing) -> throw $ Excepting.FunctionUninitializedMention f
    Just (EntryValue _) -> throw $ Excepting.FunctionNo f
    Just EntryPrimitiveFunction -> return $ Right f
    Nothing -> throw $ Excepting.FunctionUninitializedMention f

-- maybe closure or primitive function name
queryFunction' :: Name -> Execution r (Maybe (Either (Maybe Closure) Name))
queryFunction' f =
  gets (^. namespace . at f) >>= \case
    Just (EntryClosure (Just clo)) -> return . Just . Left . Just $ clo
    Just (EntryClosure Nothing) -> return . Just . Left $ Nothing
    Just (EntryValue _) -> throw $ Excepting.FunctionNo f
    Just EntryPrimitiveFunction -> return . Just . Right $ f
    Nothing -> return Nothing

-- initialize

initializeVariable :: Name -> Value -> Execution r ()
initializeVariable x v = do
  declareVariable x
  adjustVariable x v

initializeFunction :: Name -> Binding -> Execution r ()
initializeFunction f bnd = do
  declareFunction f
  adjustFunction f bnd

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
