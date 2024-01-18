{-# LANGUAGE RecordWildCards #-}
module Panini.Environment where

import Data.Function
import Data.Generics.Uniplate.Operations
import Data.List qualified as List
import Data.Map (Map)
import Data.Map qualified as Map
import Panini.Abstract.AExpr
import Panini.Abstract.AString (AString)
import Panini.Error
import Panini.Panic
import Panini.Pretty
import Panini.Provenance
import Panini.Solver.Assignment
import Panini.Solver.Constraints
import Panini.Syntax
import Prelude

-------------------------------------------------------------------------------

-- | The elaborator environment stores definitions, i.e., mappings from names
-- (of variables, constants, functions) to their types. During elaboration, the
-- state of these types might change, e.g., an inferred type might be found to
-- have an invalid verification condition during SMT solving (see 'Definition').
type Environment = Map Name Definition

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
    is 'Invalid'. If there is an κ variable assignment that validates the VC,
    then we have successfully 'Verified' the definition and are able to produce
    a fully solved type signature without any holes.

    > ╔════════╗  type inference  ╭──────────╮   VC solving   ╭──────────╮
    > ║ define ╟─┬───────────────►│ Inferred ├─┬─────────────►│ Verified │
    > ╚════════╝ │  ╭──────────╮  ╰──────────╯ │  ╭─────────╮ ╰──────────╯
    >            ╰─►│ Rejected │               ╰─►│ Invalid │
    >               ╰──────────╯                  ╰─────────╯

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
        , _error       :: Error
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
        , _error        :: Error
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

    deriving stock (Show, Read)

-------------------------------------------------------------------------------

isVerified :: Definition -> Bool
isVerified = \case
  Verified{} -> True
  _          -> False

isFailed :: Definition -> Bool
isFailed = \case
  Rejected{} -> True
  Invalid{}  -> True
  _          -> False

-------------------------------------------------------------------------------

data TypeSig = TypeSig Name Type
  deriving stock (Eq, Show, Read)

instance Pretty TypeSig where
  pretty (TypeSig x t) = pretty x <+> ":" <+> pretty t

-------------------------------------------------------------------------------

-- | All definitions in the environment, sorted by order of appearance
-- (according to provenance information).
sortedDefinitions :: Environment -> [Definition]
sortedDefinitions = 
  map snd . List.sortBy (compare `on` getPV . fst) . Map.toList

-- | Returns all type errors in the environment.
getTypeErrors :: Environment -> [Error]
getTypeErrors = map _error . filter isFailed . sortedDefinitions

-- | Return type signatures for all verified definitions in the environment.
getVerifiedTypes :: Environment -> [TypeSig]
getVerifiedTypes env = 
  [ TypeSig _name _solvedType | Verified{..} <- sortedDefinitions env]

-- | Return all verified grammars in the environment.
getVerifiedGrammars :: Environment -> [AString]
getVerifiedGrammars = concatMap extractGrammars 
                    . map (\(TypeSig _ t) -> t)
                    . getVerifiedTypes
  where    
    -- TODO: this is pretty hacky and limited
    extractGrammars :: Type -> [AString]
    extractGrammars (TFun _ t1 t2 _) = extractGrammars t1 ++ extractGrammars t2
    extractGrammars t@(TBase x TString (Known p) _) = case p of
      PRel (EVar y :∈: EStrA s) | x == y -> [s]
      _ | not $ null [True | PRel (_ :∈: _) <- universe p ] -> 
          panic $ "extractGrammars: irregular grammar:" <+> pretty t
      _ -> []
    extractGrammars _ = []
