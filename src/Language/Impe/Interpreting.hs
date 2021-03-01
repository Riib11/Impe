module Language.Impe.Interpreting where

import Control.Applicative
import Control.Lens hiding (Context)
import Control.Monad
import Data.List (intercalate)
import Data.Map as Map hiding (foldr, map)
import Data.Maybe
import qualified Language.Impe.Executing as Executing
import Language.Impe.Grammar
import Language.Impe.Primitive
import qualified Language.Impe.Typechecking as Typechecking
import Polysemy
import Polysemy.Error as Error
import Polysemy.Output as Output
import Polysemy.State as State
import Text.Printf

{-
# Interpreting

Steps:
1. parse
2. typecheck
3. execute
-}

{-
## Data
-}

data Context = Context
  { _typecheckingContext :: Typechecking.Context,
    _executionContext :: Executing.Context
  }
  deriving (Show)

type Interpretation a =
  Sem
    '[ State Context,
       Error String,
       Output String
     ]
    a

makeLenses ''Context

-- instances

-- TODO

-- interface

runInterpretation ::
  Context ->
  Interpretation a ->
  ([String], Either String (Context, a))
runInterpretation ctx =
  run
    . runOutputList
    . runError
    . runState ctx

emptyContext :: Context
emptyContext =
  Context
    { _typecheckingContext = Typechecking.emptyContext,
      _executionContext = Executing.emptyContext
    }

{-
## Computation
-}

-- TODO
-- interpretProgram :: Interpretation ()
-- interpretProgram = do
--   -- typechecking
--   tcCtx <- gets (^. typecheckingContext)
--   (tcCtx', ()) <- runState tcCtx Typechecking.typecheckProgram
--   -- executing
--   exCtx <- gets (^. executionContext)
--   (exCtx', ()) <- runState exCtx Executing.executeProgram
--   -- update context
--   modify $ typecheckingContext .~ tcCtx'
--   modify $ executionContext .~ exCtx'
