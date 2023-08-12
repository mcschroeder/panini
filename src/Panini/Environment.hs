{-# LANGUAGE RecordWildCards #-}
module Panini.Environment where

import Data.Map (Map)
import Panini.Pretty
import Panini.Solver.Assignment
import Panini.Solver.Constraints
import Panini.Syntax
import Prelude

-------------------------------------------------------------------------------

type Environment = Map Name Definition

data Definition
    = Assumed
        { _name :: Name
        , _type :: Type
        }
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

hasInferredType :: Definition -> Bool
hasInferredType = \case
  Assumed{} -> False
  Verified{_assumedType,_solvedType} -> _assumedType /= Just _solvedType

-------------------------------------------------------------------------------

data TypeSig = TypeSig Name Type
  deriving stock (Eq, Show, Read)

instance Pretty TypeSig where
  pretty (TypeSig x t) = pretty x <+> ":" <+> pretty t

toTypeSig :: Definition -> TypeSig
toTypeSig = \case
  Assumed{_name,_type} -> TypeSig _name _type
  Verified{_name,_solvedType} -> TypeSig _name _solvedType
