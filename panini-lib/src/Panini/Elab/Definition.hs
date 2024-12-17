module Panini.Elab.Definition where

import Data.Maybe
import Panini.Elab.Error
import Panini.Solver.Assignment
import Panini.Solver.Constraints
import Panini.Syntax
import Prelude

-------------------------------------------------------------------------------

{- | A 'Definition' maps a 'Name' to a 'Type' and additional related
information, such as a verification condition (VC), an underlying 'Term', or the
most recent error that occurred while elaborating.

During the elaboration process, the elaborator updates the definitions in its
environment as it goes through the phases of type inference and constraint
solving. A definition typically starts out with an unrefined or partially
refined type given by the user, perhaps including type holes, and ideally ends
up with a most specific refined type, with all holes filled in by an appropriate
assignment of κ variables, validated by a verification condition. In case of a
type checking or SMT solving error, the 'Definition' bundle contains the related
error information.

There are two types of 'Statement' that are elaborated into 'Definition's:

(1) 'Assume' statements @x : t@ define axioms that are simply trusted to be
    correct. The given type @t@ never changes (it is 'Assumed').

    > ╔════════╗    ╭─────────╮
    > ║ assume ╟───►│ Assumed │
    > ╚════════╝    ╰─────────╯

(2) 'Define' statements @x = e@ come with a 'Term' @e@ that needs to go trough
    type inference. If there is a type error, the definition is 'Rejected';
    otherwise, a type together with a VC is 'Inferred', maybe based on a
    previously assumed type for the same name. Both the inferred type and the VC
    will contain κ variables, for which a satisfying assignment needs to be
    found before the VC can be discharged by an SMT solver. If this is not
    possible, or the VC is unsatisfiable for other reasons, then the definition
    is 'Invalid'. If there is a κ variable assignment that validates the VC,
    then we have successfully 'Verified' the definition and are able to produce
    a fully solved type signature without any holes. Occasionally, the SMT
    solver is not able to make a decision one way or another (e.g., it might
    time out), resulting in an 'Unverified' definition which contains a fully
    solved yet unproven type signature.

    > ╔════════╗  type inference  ╭──────────╮   VC solving      ╭──────────╮
    > ║ define ╟─┬───────────────►│ Inferred ├─┬──────────────┬─►│ Verified │
    > ╚════════╝ │  ╭──────────╮  ╰──────────╯ │  ╭─────────╮ │  ╰──────────╯
    >            ╰─►│ Rejected │               ╰─►│ Invalid │ │  ╭────────────╮
    >               ╰──────────╯                  ╰─────────╯ ╰─►│ Unverified │
    >                                                            ╰────────────╯

-}
data Definition
    -- | User-defined axioms are trusted to be correct.
    = Assumed
        { _name :: Name
        , _type :: Type
        }

    -- | We reject definitions if they have type errors.
    | Rejected
        { _name        :: Name
        , _assumedType :: Maybe Type
        , _term        :: Term
        , _error       :: ElabError  -- TODO: TypeError
        }
    
    -- | An inferred type may be based on a previously assumed type. Every
    -- inferred type comes with a verification condition (VC) and both the
    -- inferred type and the VC may contain κ variables. At this point, we don't
    -- know yet if there is an assignment for all κ variable that would validate
    -- the VC.
    | Inferred
        { _name         :: Name
        , _assumedType  :: Maybe Type
        , _term         :: Term
        , _inferredType :: Type
        , _vc           :: Con
        }

    -- | If we cannot find a satisfying κ assignment, or encounter other
    -- problems in trying to solve the VC, the definition must be invalid.
    | Invalid
        { _name         :: Name
        , _assumedType  :: Maybe Type
        , _term         :: Term
        , _inferredType :: Type
        , _vc           :: Con
        , _error        :: ElabError
        }

    -- A verified definition has an assignment for all κ variables that
    -- validates the VC and fills all holes of the inferred type, resulting in a
    -- fully solved maximally refined type.
    | Verified
        { _name         :: Name
        , _assumedType  :: Maybe Type
        , _term         :: Term
        , _inferredType :: Type
        , _vc           :: Con
        , _solution     :: Assignment
        , _solvedType   :: Type
        }
    
    -- An unverified definition appears if the SMT solver is unable to determine
    -- the validity of an inferred solution (e.g., it timed out).
    | Unverified
        { _name         :: Name
        , _assumedType  :: Maybe Type
        , _term         :: Term
        , _inferredType :: Type
        , _vc           :: Con
        , _solution     :: Assignment
        , _solvedType   :: Type
        , _reason       :: String
        }

-------------------------------------------------------------------------------

isFailed :: Definition -> Bool
isFailed = \case
  Rejected{} -> True
  Invalid{}  -> True
  _          -> False

isSolved :: Definition -> Bool
isSolved = \case
  Verified{}   -> True
  Unverified{} -> True
  _            -> False
