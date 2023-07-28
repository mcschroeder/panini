module Panini.Language.Environment where

import Data.Map (Map)
import Panini.Language.AST
import Panini.Logic.Constraints
import Panini.Logic.Solver.Assignment
import Panini.Names
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
