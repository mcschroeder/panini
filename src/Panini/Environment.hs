module Panini.Environment where

import Data.Map (Map)
import Panini.Logic.Assignment
import Panini.Logic.Constraints
import Panini.Syntax
import Prelude

-------------------------------------------------------------------------------

type Environment = Map Name Definition

data Definition
    = Assumed
        { _name      :: Name
        , _givenType :: Type
        }
    | Verified
        { _name         :: Name
        , _givenType    :: Type
        , _givenTerm    :: Term
        , _inferredType :: Type
        , _vc           :: Con
        , _solution     :: Assignment
        , _solvedType   :: Type
        }

    deriving stock (Show, Read)
