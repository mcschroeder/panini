module Panini.Environment where

import Data.Map (Map)
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
        , _givenTerm    :: Term
        , _inferredType :: Type
        , _vc           :: Con
        , _solution     :: Assignment
        , _solvedType   :: Type
        }

    deriving stock (Show, Read)
