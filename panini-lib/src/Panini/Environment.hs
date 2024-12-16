{-# LANGUAGE RecordWildCards #-}
module Panini.Environment where

import Data.Function
import Data.List qualified as List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe
import Panini.Diagnostic
import Panini.Pretty
import Panini.Provenance
import Panini.Solver.Assignment
import Panini.Solver.Constraints
import Panini.Syntax
import Panini.SMT.Error qualified as SMT
import Prelude
import Panini.Abstract.AValue
import Panini.Parser qualified

------------------------------------------------------------------------------

-- TODO: move these out of here once the circular dependency is gone

data ElabError where
  AlreadyDefined  :: Name -> ElabError
  Unsolvable      :: Name -> Con -> ElabError
  SolverError     :: SolverError -> ElabError
  TypeError       :: TypeError -> ElabError
  ParseError      :: Panini.Parser.Error -> ElabError
  IOError         :: IOError -> ElabError

data SolverError where
  AbstractionToValueImpossible  :: Name -> ARel -> AValue -> SolverError
  SmtSolverError                :: SMT.Error -> SolverError

data TypeError where
  UnknownVar        :: Name -> TypeError
  InvalidSubtype    :: Type -> Type -> TypeError
  ExpectedFunType   :: Term -> Type -> TypeError

instance Diagnostic TypeError where
  diagnosticMessage = \case
    UnknownVar x         -> "unknown variable" <\> pretty x
    InvalidSubtype t1 t2 -> "invalid subtype:" <\> pretty t1 <+> "<:" <+> pretty t2
    ExpectedFunType _ t  -> "invalid function type:" <\> pretty t
 
instance HasProvenance TypeError where
  getPV = \case
    UnknownVar x        -> getPV x
    InvalidSubtype t _  -> getPV t
    ExpectedFunType e _ -> getPV e
  setPV = undefined
  
instance Diagnostic SolverError where
  diagnosticMessage = \case
    AbstractionToValueImpossible x r e ->
      "abstraction to value impossible:" <\> 
      "⟦" <> pretty r <> "⟧↑" <> pretty x <+> "≐" <+> pretty e
    SmtSolverError e -> diagnosticMessage e

instance HasProvenance SolverError where  
  getPV = \case
    AbstractionToValueImpossible x _r1 _ -> getPV x -- TODO: getPV r1
    SmtSolverError _ -> NoPV
  setPV = undefined

instance Diagnostic ElabError where
  diagnosticMessage = \case
    AlreadyDefined x -> "multiple definitions for" <\> pretty x
    Unsolvable x _ -> "cannot solve constraints of" <\> pretty x
    TypeError e -> diagnosticMessage e
    SolverError e -> diagnosticMessage e
    ParseError e -> diagnosticMessage e
    IOError e -> diagnosticMessage e

instance HasProvenance ElabError where
  getPV = \case
    AlreadyDefined x -> getPV x
    Unsolvable x _ -> getPV x
    TypeError e -> getPV e
    SolverError e -> getPV e
    ParseError e -> getPV e
    IOError _ -> NoPV
  setPV = undefined

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

-------------------------------------------------------------------------------

-- | A type signature, with an optional comment.
data TypeSig = TypeSig Name Type (Maybe String)
  deriving stock (Eq, Show, Read)

instance Pretty TypeSig where
  pretty (TypeSig x t c) = pretty x <+> ":" <+> pretty t <> comment
   where
    comment | Just m <- c = "  " <> ann Comment ("--" <+> pretty m)
            | otherwise   = mempty

-------------------------------------------------------------------------------

-- | All definitions in the environment, sorted by order of appearance
-- (according to provenance information).
sortedDefinitions :: Environment -> [Definition]
sortedDefinitions = 
  map snd . List.sortBy (compare `on` getPV . fst) . Map.toList

-- | Returns all type errors in the environment.
getTypeErrors :: Environment -> [ElabError]
getTypeErrors = map _error . filter isFailed . sortedDefinitions

-- | Return type signatures for all fully solved definitions in the environment,
-- both verified and unverified.
getSolvedTypes :: Environment -> [TypeSig]
getSolvedTypes = catMaybes . map go . sortedDefinitions
 where
  go Verified{..}   = Just $ TypeSig _name _solvedType Nothing
  go Unverified{..} = Just $ TypeSig _name _solvedType (Just comment)
                        where comment = "UNVERIFIED (" ++ _reason ++ ")"
  go _ = Nothing
