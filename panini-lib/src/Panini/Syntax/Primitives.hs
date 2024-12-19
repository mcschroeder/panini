module Panini.Syntax.Primitives where

import Data.Data (Data, toConstr, constrIndex)
import Data.Generics.Uniplate.Direct
import Data.Hashable
import Data.Text (Text)
import GHC.Generics
import Panini.Pretty
import Panini.Provenance
import Prelude

------------------------------------------------------------------------------

-- | Primitive base types.
data Base
  = TUnit
  | TBool
  | TInt
  | TChar
  | TString
  deriving stock (Ord, Eq, Show, Read, Generic, Data)

instance Hashable Base

instance Pretty Base where
  pretty TUnit   = symTUnit
  pretty TBool   = symTBool
  pretty TInt    = symTInt
  pretty TChar   = symTChar
  pretty TString = symTString

------------------------------------------------------------------------------

-- | Primitive values.
data Value
  = U          !PV  -- unit
  | B !Bool    !PV  -- true, false
  | I !Integer !PV  -- 0, -1, 1, ...
  | C !Char    !PV  -- 'a'
  | S !Text    !PV  -- "lorem ipsum"
  deriving stock (Show, Read, Data)

-- | Equality between values ignores provenance.
instance Eq Value where
  U   _ == U   _ = True
  B a _ == B b _ = a == b
  I a _ == I b _ = a == b
  C a _ == C b _ = a == b
  S a _ == S b _ = a == b
  _     == _     = False

-- | Structural ordering between values; ignores provenance.
instance Ord Value where
  U   _ <= U   _ = True
  B a _ <= B b _ = a <= b
  I a _ <= I b _ = a <= b
  C a _ <= C b _ = a <= b
  S a _ <= S b _ = a <= b
  a     <= b     = constrIndex (toConstr a) <= constrIndex (toConstr b)

-- | Hashing values ignores provenance.
instance Hashable Value where
  hashWithSalt s (U   _) = s `hashWithSalt` (0 :: Int)
  hashWithSalt s (B a _) = s `hashWithSalt` (1 :: Int) `hashWithSalt` a
  hashWithSalt s (I a _) = s `hashWithSalt` (2 :: Int) `hashWithSalt` a
  hashWithSalt s (C a _) = s `hashWithSalt` (3 :: Int) `hashWithSalt` a
  hashWithSalt s (S a _) = s `hashWithSalt` (4 :: Int) `hashWithSalt` a

instance HasProvenance Value where
  getPV (U   pv) = pv
  getPV (B _ pv) = pv
  getPV (I _ pv) = pv
  getPV (C _ pv) = pv
  getPV (S _ pv) = pv

instance Uniplate Value where
  uniplate = plate

instance Pretty Value where
  pretty (U       _) = symUnit
  pretty (B True  _) = symTrue
  pretty (B False _) = symFalse
  pretty (I c     _) = ann (Literal NumberLit) $ pretty c
  pretty (C c     _) = ann (Literal CharLit) $ viaShow c
  pretty (S t     _) = ann (Literal StringLit) $ viaShow t

typeOfValue :: Value -> Base
typeOfValue = \case
  U _   -> TUnit 
  B _ _ -> TBool 
  I _ _ -> TInt
  C _ _ -> TChar
  S _ _ -> TString
