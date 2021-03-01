module Language.Impe.Executing where

import Control.Applicative
import Control.Lens hiding (Context)
import Control.Monad
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import Data.Namespace as Namespace
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

data Context = Context
  { _namespace :: Namespace Name (Either (Maybe Value) (Maybe Closure)),
    _inputs :: [String],
    _outputs :: [String]
  }

type Value = Expression

type Binding = ([Name], Instruction)

type Closure = (Binding, Scope Name)

type Execution a =
  Sem
    '[ State Context,
       Error String,
       Output String
     ]
    a

makeLenses ''Context

-- instances

instance Show Context where
  show ctx =
    unlines
      [ "execution context:",
        "  namespace:",
        "    scope:",
        printf "      %s" (show $ ctx ^. namespace . scope . to NonEmpty.toList),
        "    store:",
        unlines
          . map
            ( \((x, i), e) -> case e of
                Left mb_val -> case mb_val of
                  Just val ->
                    printf "      var %s#%s = %s" (show x) (show i) (show val)
                  Nothing ->
                    printf "      var %s undefined" (show x)
                Right mb_clo -> case mb_clo of
                  Just ((xs, inst), _) ->
                    printf "      fun %s(%s) = %s" (show x) (List.intercalate ", " . map show $ xs) (show inst)
                  Nothing ->
                    printf "      fun %s undefined" (show x)
            )
          $ ctx ^. namespace . store . to Map.toList,
        "    inputs:",
        "      " ++ List.intercalate ("\n      ") (ctx ^. inputs),
        "    outputs:",
        "      " ++ List.intercalate ("\n      ") (ctx ^. outputs)
      ]

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
          updateVariable x undefined -- TODO
    )
    primitive_variables
  mapM_
    ( \(f, params, _) -> do
        declareFunction f
        updateFunction f (fst <$> params, PrimitiveFunctionBody f (fst <$> params))
    )
    primitive_functions

executeMain :: Execution ()
executeMain = do
  queryFunction' mainName >>= \case
    Just (Just (([], _), _)) ->
      void $ executeInstruction (ProcedureCall mainName [])
    Just (Just ((_, _), _)) ->
      throw $ "[type prohibited] the main function must take 0 arguments"
    Just Nothing ->
      throw $ "the main function was declared but not defined"
    Nothing ->
      return ()
  return ()

{-
## Execution
-}

executeInstruction :: Instruction -> Execution (Maybe Value)
executeInstruction = \case
  Block insts -> locallyScoped do
    foldl (<|>) Nothing <$> traverse executeInstruction insts
  Declaration x _ -> do
    declareVariable x
    return Nothing
  Assignment x e -> do
    updateVariable x =<< evaluateExpression e
    return Nothing
  Function f params _ inst -> do
    declareFunction f
    updateFunction f (fst <$> params, inst)
    return Nothing
  Conditional e inst1 inst2 ->
    evaluateExpression e >>= \case
      Bool True -> locallyScoped $ executeInstruction inst1
      Bool False -> locallyScoped $ executeInstruction inst2
      _ -> throw $ printf "the condition `%s` must be of type `%s`." (show e) (show BoolType)
  Loop e inst ->
    evaluateExpression e >>= \case
      Bool True -> locallyScoped $ executeInstruction $ Loop e inst
      Bool False -> return Nothing
      _ -> throw $ printf "the condition `%s` must be of type `%s`." (show e) (show BoolType)
  Return e ->
    Just <$> evaluateExpression e
  ProcedureCall f args -> do
    ((xs, inst), scp) <- queryFunction f

    withScope scp do
      mapM_
        ( \(x, e) -> do
            v <- evaluateExpression e
            declareVariable x
            updateVariable x v
        )
        (zip xs args)
      void $ executeInstruction inst
      return Nothing
  PrimitiveFunctionBody f xs ->
    executePrimitiveFunctionBody f xs

executePrimitiveFunctionBody :: Name -> [Name] -> Execution (Maybe Value)
executePrimitiveFunctionBody f xs = do
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
      writeOutput (show v)
      return Nothing
    (Name "output_int", [Int v]) -> do
      writeOutput (show v)
      return Nothing
    _ -> throw $ printf "the primitive function `%s` was not recognized" (show f)

{-
## Evaluation
-}

evaluateInstruction :: Instruction -> Execution Value
evaluateInstruction inst =
  executeInstruction inst >>= \case
    Just v -> return v
    Nothing -> throw $ printf "expected instruction `%s` to return a value." (show inst)

evaluateExpression :: Expression -> Execution Value
evaluateExpression = \case
  Reference x -> queryVariable x
  Application f es -> do
    ((xs, inst), scp) <- queryFunction f
    withScope scp do
      mapM_
        ( \(x, e) -> do
            v <- evaluateExpression e
            declareVariable x
            updateVariable x v
        )
        (zip xs es)
      evaluateInstruction inst
  v -> return v

{-
## Namespace
-}

-- scoping

locallyScoped :: Execution a -> Execution a
locallyScoped exe = do
  modify $ namespace %~ enterScope -- enter local scope
  a <- exe
  modify $ namespace %~ leaveScope -- leave local scope
  return a

withScope :: Scope Name -> Execution a -> Execution a
withScope scp exe = do
  scpOri <- gets (^. namespace . scope)
  modify $ namespace . scope .~ scp -- adopt new scope
  a <- exe
  modify $ namespace . scope .~ scpOri -- resert original scope
  return a

-- declare

declareVariable :: Name -> Execution ()
declareVariable x = modify $ namespace . at x .~ Just (Left Nothing)

declareFunction :: Name -> Execution ()
declareFunction f = modify $ namespace . at f .~ Just (Right Nothing)

-- update

updateVariable :: Name -> Value -> Execution ()
updateVariable x val =
  gets (^. namespace . at x) >>= \case
    Just (Left _) ->
      modify $ namespace . at x .~ Just ((Left . Just) val)
    Just (Right _) ->
      throw $ printf "expected `%s` to be a variable, but it is actually a function" (show x)
    Nothing ->
      modify $ namespace . at x .~ Just ((Left . Just) val)

updateFunction :: Name -> Binding -> Execution ()
updateFunction f bnd =
  gets (^. namespace . at f) >>= \case
    Just (Right _) -> do
      scp <- gets (^. namespace . scope)
      modify $ namespace . at f .~ Just ((Right . Just) (bnd, scp))
    Just (Left _) ->
      throw $ printf "expected `%s` to be a function, but it is actually a variable" (show f)
    Nothing -> do
      scp <- gets (^. namespace . scope)
      modify $ namespace . at f .~ Just ((Right . Just) (bnd, scp))

-- query

queryVariable :: Name -> Execution Value
queryVariable x =
  gets (^. namespace . at x) >>= \case
    Just (Left (Just val)) ->
      return val
    Just (Left Nothing) ->
      throw $ printf "the variable `%s` cannot be queried before it has a value" (show x)
    Just (Right _) ->
      throw $ printf "expected `%s` to be a variable, but it is actually a function" (show x)
    Nothing ->
      throw $ printf "the variable `%s` cannot be queried before its declaration" (show x)

queryVariable' :: Name -> Execution (Maybe (Maybe Value))
queryVariable' x =
  gets (^. namespace . at x) >>= \case
    Just (Left (Just val)) ->
      return $ Just (Just val)
    Just (Left Nothing) ->
      return $ Just Nothing
    Just (Right _) ->
      throw $ printf "expected `%s` to be a variable, but it is actually a function" (show x)
    Nothing ->
      return Nothing

queryFunction :: Name -> Execution Closure
queryFunction f =
  gets (^. namespace . at f) >>= \case
    Just (Right (Just clo)) -> return clo
    Just (Right Nothing) -> throw $ printf "the function `%s` cannot be queried before it has a value" (show f)
    Just (Left _) -> throw $ printf "expected `%s` to be a function, but it is actually a variable" (show f)
    Nothing -> throw $ printf "the function `%s` cannot be queried before its declaration" (show f)

queryFunction' :: Name -> Execution (Maybe (Maybe Closure))
queryFunction' f =
  gets (^. namespace . at f) >>= \case
    Just (Right (Just clo)) -> return $ Just (Just clo)
    Just (Right Nothing) -> return $ Just Nothing
    Just (Left _) -> throw $ printf "expected `%s` to be a function, but it is actually a variable" (show f)
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
