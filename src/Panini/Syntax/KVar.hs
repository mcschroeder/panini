module Panini.Syntax.KVar where

import Data.Data (Data)
import Data.Hashable
import Data.String
import GHC.Generics (Generic)
import Panini.Pretty
import Panini.Provenance
import Panini.Syntax.Names
import Panini.Syntax.Primitives
import Prelude

------------------------------------------------------------------------------

-- | A /refinement variable/ κ represents an unknown refinement over some free
-- variables z₁,z₂,…,zₙ, known as the /parameters/ of κ. For any κ-variable, we
-- know its arity (the number of parameters) as well as the expected parameter
-- types τ₁,τ₂,…,τₙ.
-- 
-- Refinement variables can occur in predicates in the form of applications
-- ('PAppK') which bind the parameters to particular values. Note that the
-- refinement represented by an applied κ-variable is unknown until the solving
-- phase, where we aim to find some assignment σ that maps each κ-variable to a
-- concrete refinement predicate satisfying the constraints induced by the
-- applications.
--
-- In the literature, κ-variables are sometimes referred to as /Horn variables/.
data KVar = KVar !Int ![Base] !PV
  deriving stock (Ord, Eq, Show, Read, Generic, Data)

instance Hashable KVar

-- | The parameters of a κ-variable.
kparams :: KVar -> [Name]
kparams (KVar _ ts _) = [fromString $ "z" ++ show @Int i | i <- [0..length ts-1]]

ktypes :: KVar -> [Base]
ktypes (KVar _ ts _) = ts

instance Pretty KVar where
  pretty k@(KVar _ ts _) = prettyKVarName k <> prettyTuple zs
    where
      zs = [pretty z <> colon <> pretty t | (z,t) <- zip (kparams k) ts]

prettyKVarName :: KVar -> Doc
prettyKVarName (KVar i _ _) = ann (Identifier VarIdent) $ kappa <> subscript i

instance HasProvenance KVar where
  getPV (KVar _ _ pv) = pv
  setPV pv (KVar i ts _) = KVar i ts pv
