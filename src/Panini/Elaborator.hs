{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Panini.Elaborator where

import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Strict
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text.IO qualified as Text
import Panini.Error
import Panini.Parser
import Panini.Solver.Assignment
import Panini.Syntax
import Panini.TypeChecker
import Prelude
import Panini.Solver.Fusion qualified

-------------------------------------------------------------------------------

-- | Elaborator monad.
type Elab = StateT ElabState (ExceptT Error IO)

-- | Throw an `Error` in the elaborator monad.
throwError :: Error -> Elab ()
throwError = lift . throwE

-- | Catch an `Error` in the elaborator monad.
catchError :: Elab a -> (Error -> Elab a) -> Elab a
catchError = liftCatch catchE

-- | Try an elaborator action and return any thrown `Error`.
tryError :: Elab a -> Elab (Either Error a)
tryError m = catchError (Right <$> m) (return . Left)

-- | Elaborator state.
data ElabState = ElabState
  { pan_types :: Ctx            -- ^ global typing context (Gamma)
  , pan_terms :: Map Name Term  -- ^ top-level functions
  , pan_vcs :: Map Name Con    -- ^ verification conditions
  , environment :: Environment
  }

-- | Initial (empty) elaborator state.
initState :: ElabState
initState = ElabState 
  { pan_types = Map.empty 
  , pan_terms = Map.empty
  , pan_vcs = Map.empty
  , environment = mempty
  }

-------------------------------------------------------------------------------

-- | The elaborator environment stores definitions, i.e., mappings from names
-- (of variables, constants, functions) to their types. During elaboration, the
-- state of these types might change, e.g., an inferred type might be found to
-- be unsatisfiable during SMT solving. See the definition of 'Definition' for a
-- better definition.
type Environment = Map Name Definition

-- | Retrieve a definition from the environment.
envLookup :: Name -> Elab (Maybe Definition)
envLookup x = Map.lookup x <$> gets environment

-- | Extend the environment with a new definition.
envExtend :: Name -> Definition -> Elab ()
envExtend x d = modify' $ \s -> s { environment = Map.insert x d s.environment }

-- | Remove a definition from the environment.
envDelete :: Name -> Elab ()
envDelete x = modify' $ \s -> s { environment = Map.delete x s.environment }


{- | A 'Definition' maps a 'Name' to a 'Type' and additional related
information, such as verification conditions (VCs), an underlying 'Term', or
the most recent error that occurred while elaborating.

During the elaboration process, the elaborator updates the definitions in its
environment as it goes through the phases of refinement type inference and SMT
solving. A definition typically starts out with an unrefined type given by the
user and ideally ends up with a most specific refined type plus VC, together
with a satisfying Horn variable assignment. In case of a type checking or SMT
solving error, the 'Definition' bundle contains the related error information.

There are two types of 'Statement' that are elaborated into 'Definition's:

(1) 'Assume' statements (@assume x : t@) define axioms that are simply trusted
    to be correct. The given type (@t@) never changes (it is 'Assumed').

    > ╔════════╗    ╭─────────╮
    > ║ assume ╟───►│ Assumed │
    > ╚════════╝    ╰─────────╯

(2) 'Define' statements (@define x : t = e@) come with a 'Term' (@e@) that needs
    to go trough type inference. If there is a type error, the definition is
    'Rejected'; otherwise, the (usually unrefined) given type (@t@) is
    'Inferred' to a more specific (most refined) type with a VC. The VC needs to
    be discharged by an SMT solver. At this stage, the VC, as well as the
    refined type, will most likely contain Horn variables. If we can not find a
    satisfying assignment for these Horn variables, or the VC is unsatisfiable
    for other reasons, then the definition is 'Invalid'. If there is a
    satisfying assignment, then we have successfully 'Verified' the definition.

    > ╔════════╗  type inference  ╭──────────╮  Horn solving  ╭──────────╮
    > ║ define ╟─┬───────────────►│ Inferred ├─┬─────────────►│ Verified │
    > ╚════════╝ │  ╭──────────╮  ╰──────────╯ │  ╭─────────╮ ╰──────────╯
    >            ╰─►│ Rejected │               ╰─►│ Invalid │
    >               ╰──────────╯                  ╰─────────╯

-}
data Definition
  -- | User-defined axioms are trusted to be correct.
  = Assumed 
      { _name :: Name
      , _givenType :: Type 
      }
    
  -- | We reject definitions if they have type errors.
  | Rejected
      { _name :: Name
      , _givenType :: Type 
      , _givenTerm :: Term 
      , _typeError :: Error
      }
  
  -- | Every inferred type (which might be different than the given type) comes
  -- with a verification condition (VC). Both the inferred type and the VC may
  -- contain Horn variables. At this point, we don't know yet if the VC is
  -- satisfiable and the inferred type correct.
  | Inferred
      { _name :: Name
      , _givenType :: Type 
      , _givenTerm :: Term 
      , _inferredType :: Type
      , _vc :: Con
      }

  -- | If we can not find a satisfying Horn assignment for the VC, or encounter
  -- other problems in trying to solve the VC, the inferred type is incorrect
  -- and the definition as a whole is invalid.
  | Invalid
      { _name :: Name
      , _givenType :: Type 
      , _givenTerm :: Term 
      , _inferredType :: Type
      , _vc :: Con
      , _solverMsg :: Maybe String
      }
  
  -- | A verified definition comes with a satisfying assignment for all Horn
  -- variables in the VC and the inferred type.
  | Verified
      { _name :: Name
      , _givenType :: Type 
      , _givenTerm :: Term 
      , _inferredType :: Type
      , _vc :: Con
      , _solution :: Assignment
      }
  
  deriving stock (Show, Read)


-- | Convert an elaborator environment to a typechecking context by throwing
-- away all non-final definitions.
envToContext :: Environment -> Ctx 
envToContext = Map.mapMaybe go
  where
    go (Assumed   {_givenType})    = Just _givenType
    go (Verified  {_inferredType}) = Just _inferredType
    go _                           = Nothing

-------------------------------------------------------------------------------

elaborateProgram :: Program -> Elab ()
elaborateProgram = mapM_ elaborateStatement

elaborateStatement :: Statement -> Elab ()
elaborateStatement = \case
  Assume x t -> do
    def0 <- envLookup x
    case def0 of
      Just _  -> throwError $ AlreadyDefined x
      Nothing -> envExtend x (Assumed x t)    
  
  Define x t0 e -> do
    def0 <- envLookup x
    case def0 of
      Just _  -> throwError $ AlreadyDefined x
      Nothing -> do
        g <- envToContext <$> gets environment
        let g' = Map.insert x t0 g
        case runTC $ synth g' e of
          Left err -> do
            envExtend x (Rejected x t0 e err)
            throwError err -- ?
          Right (vc,t) -> do
            envExtend x (Inferred x t0 e t vc)
            r <- liftIO $ Panini.Solver.Fusion.sat vc []
            case r of
              True -> envExtend x (Verified x t0 e t vc mempty)  -- TODO: assignment
              False -> envExtend x (Invalid x t0 e t vc Nothing)
  
  Import m -> do
    src <- liftIO $ Text.readFile m  -- TODO: handle error
    case parseProgram m src of
      Left  err  -> throwError err
      Right prog -> elaborateProgram prog
