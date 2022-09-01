module Panini.Elaborator.Environment where

import Data.Map (Map)
import Data.Map qualified as Map
import Panini.Error
import Panini.Infer
import Panini.Solver.Assignment
import Panini.Syntax
import Prelude

-------------------------------------------------------------------------------

-- | The elaborator environment stores definitions, i.e., mappings from names
-- (of variables, constants, functions) to their types. During elaboration, the
-- state of these types might change, e.g., an inferred type might be found to
-- be unsatisfiable during SMT solving. See the definition of 'Definition' for a
-- better definition.
type Environment = Map Name Definition

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
      , _givenTerm :: Term Untyped
      , _typeError :: Error
      }
  
  -- | Every inferred type (which might be different than the given type) comes
  -- with a verification condition (VC). Both the inferred type and the VC may
  -- contain Horn variables. At this point, we don't know yet if the VC is
  -- satisfiable and the inferred type correct.
  | Inferred
      { _name :: Name
      , _givenType :: Type 
      , _givenTerm :: Term Untyped
      , _inferredType :: Type
      , _vc :: Con
      }

  -- | If we can not find a satisfying Horn assignment for the VC, or encounter
  -- other problems in trying to solve the VC, the inferred type is incorrect
  -- and the definition as a whole is invalid.
  | Invalid
      { _name :: Name
      , _givenType :: Type 
      , _givenTerm :: Term Untyped
      , _inferredType :: Type
      , _vc :: Con
      , _solverMsg :: Maybe String
      }
  
  -- | A verified definition comes with a satisfying assignment for all Horn
  -- variables in the VC and the inferred type.
  | Verified
      { _name :: Name
      , _givenType :: Type 
      , _givenTerm :: Term Untyped
      , _inferredType :: Type
      , _vc :: Con
      , _solution :: Assignment
      }
  
  deriving stock (Show, Read)


-- | Convert an elaborator environment to a typechecking context by throwing
-- away all non-final definitions.
envToContext :: Environment -> Context 
envToContext = Map.mapMaybe go
  where
    go (Assumed   {_givenType})    = Just _givenType
    go (Verified  {_inferredType}) = Just _inferredType
    go _                           = Nothing
