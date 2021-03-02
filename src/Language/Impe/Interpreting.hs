module Language.Impe.Interpreting where

import Control.Lens hiding (Context)
import qualified Language.Impe.Executing as Executing
import Language.Impe.Grammar
import qualified Language.Impe.Typechecking as Typechecking
import Polysemy
import Polysemy.Error as Error
import Polysemy.Output as Output
import Polysemy.State as State

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

type Interpretation a =
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
      [ "interpretation context:",
        (unlines . map ("  " ++) . lines . show)
          (ctx ^. typecheckingContext),
        (unlines . map ("  " ++) . lines . show)
          (ctx ^. executionContext)
      ]

-- interface

emptyContext :: Context
emptyContext =
  Context
    { _typecheckingContext = Typechecking.emptyContext,
      _executionContext = Executing.emptyContext
    }

{-
## Computation
-}

interpretProgram :: Program -> Interpretation ()
interpretProgram prgm = do
  -- typechecking
  tchCtx <- gets (^. typecheckingContext)
  tchCtx' <- raise . execState tchCtx $ Typechecking.typecheckProgram prgm
  -- execution
  exeCtx <- gets (^. executionContext)
  exeCtx' <- raise . execState exeCtx $ Executing.executeProgram prgm
  -- update
  modify $ typecheckingContext .~ tchCtx'
  modify $ executionContext .~ exeCtx'
