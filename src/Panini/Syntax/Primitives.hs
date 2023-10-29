module Panini.Syntax.Primitives where

import Data.Generics.Uniplate.Direct
import Data.Hashable
import Data.Text (Text)
import GHC.Generics
import Panini.Pretty
import Panini.Provenance
import Panini.Syntax.Names
import Prelude

------------------------------------------------------------------------------

-- | Primitive base types.
data Base
  = TUnit
  | TBool
  | TInt
  | TString
  deriving stock (Ord, Eq, Show, Read, Generic)

instance Hashable Base

instance Pretty Base where
  pretty TUnit   = symTUnit
  pretty TBool   = symTBool
  pretty TInt    = symTInt
  pretty TString = symTString

------------------------------------------------------------------------------

-- | Primitive constants.
data Constant
  = U          !PV  -- unit
  | B !Bool    !PV  -- true, false
  | I !Integer !PV  -- 0, -1, 1, ...
  | S !Text    !PV  -- "lorem ipsum"
  deriving stock (Show, Read)

-- | Equality between constants ignores provenance.
instance Eq Constant where
  U   _ == U   _ = True
  B a _ == B b _ = a == b
  I a _ == I b _ = a == b
  S a _ == S b _ = a == b
  _     == _     = False

-- | Hashing constants ignores provenance.
instance Hashable Constant where
  hashWithSalt s (U   _) = s `hashWithSalt` (0 :: Int)
  hashWithSalt s (B a _) = s `hashWithSalt` (1 :: Int) `hashWithSalt` a
  hashWithSalt s (I a _) = s `hashWithSalt` (2 :: Int) `hashWithSalt` a
  hashWithSalt s (S a _) = s `hashWithSalt` (3 :: Int) `hashWithSalt` a

instance HasProvenance Constant where
  getPV (U   pv) = pv
  getPV (B _ pv) = pv
  getPV (I _ pv) = pv
  getPV (S _ pv) = pv
  setPV pv (U   _) = U pv
  setPV pv (B x _) = B x pv
  setPV pv (I x _) = I x pv
  setPV pv (S x _) = S x pv

instance Pretty Constant where
  pretty (U       _) = symUnit
  pretty (B True  _) = symTrue
  pretty (B False _) = symFalse
  pretty (I c     _) = ann (Literal NumberLit) $ pretty c
  pretty (S t     _) = ann (Literal StringLit) $ viaShow t

typeOfConstant :: Constant -> Base
typeOfConstant = \case
  U _   -> TUnit 
  B _ _ -> TBool 
  I _ _ -> TInt
  S _ _ -> TString

------------------------------------------------------------------------------

-- | A value is either a primitive constant or a variable.
data Value
  = Con !Constant 
  | Var !Name
  deriving stock (Eq, Show, Read, Generic)

instance Hashable Value

instance Pretty Value where
  pretty (Con c) = pretty c
  pretty (Var x) = pretty x

instance Uniplate Value where
  uniplate = plate

typeOfValue :: Value -> Maybe Base
typeOfValue = \case
  Con c -> Just $ typeOfConstant c
  Var _ -> Nothing
