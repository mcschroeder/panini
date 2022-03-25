{-# LANGUAGE OverloadedStrings #-}

module Panini.Syntax where

import Data.Char
import Data.String
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Read
import Panini.Provenance
import Prelude

------------------------------------------------------------------------------
-- Names

-- | A name for something, e.g. a variable or a binder.
data Name = Name Text PV
  deriving stock (Show, Read)

instance HasProvenance Name where
  getPV (Name _ pv) = pv
  setPV pv (Name x _) = Name x pv

-- | Equality between names ignores provenance.
instance Eq Name where
  Name a _ == Name b _ = a == b

-- | Ordering between names ignores provenance.
instance Ord Name where
  Name a _ <= Name b _ = a <= b

instance IsString Name where
  fromString s = Name (Text.pack s) NoPV

dummyName :: Name
dummyName = "_"

isDummy :: Name -> Bool
isDummy n = n == dummyName

-- | Returns a fresh name based on a given name, avoiding unwanted names.
freshName :: Name -> [Name] -> Name
freshName x ys = go (nextSubscript x)
 where
  go x'
    | x' `elem` ys = go (nextSubscript x')
    | otherwise    = x'

-- | Given "x", returns "x1"; given "x1", returns "x2"; and so on.
nextSubscript :: Name -> Name
nextSubscript (Name n _) = case decimal @Int $ Text.takeWhileEnd isDigit n of
  Left _      -> Name (Text.append n "1") NoPV
  Right (i,_) -> Name (Text.append n $ Text.pack $ show (i + 1)) NoPV

------------------------------------------------------------------------------
-- Top-level declarations

type Prog = [Decl]

data Decl
  = Assume Name Type   -- assume x : t
  | Define Name Term   -- define x = e
  deriving stock (Show, Read)

------------------------------------------------------------------------------
-- Terms

data Term
  = Val Value                -- x
  | App Term Value           -- e x
  | Lam Name Term            -- \x. e
  | Ann Term Type            -- e : t
  | Let Name Term Term       -- let x = e1 in e2
  | Rec Name Type Term Term  -- rec x : t = e1 in e2
  | If Value Term Term       -- if x then e1 else e2
  deriving stock (Show, Read)

------------------------------------------------------------------------------
-- Values

data Value
  = U PV          -- unit
  | B Bool PV     -- true, false
  | I Integer PV  -- 0, -1, 1, ...
  | S Text PV     -- "lorem ipsum"
  | V Name        -- x
  deriving stock (Show, Read)

-- | Equality between values ignores provenance.
instance Eq Value where
  U   _ == U   _ = True
  B a _ == B b _ = a == b
  I a _ == I b _ = a == b
  S a _ == S b _ = a == b
  V a   == V b   = a == b
  _     == _     = False

instance HasProvenance Value where
  getPV (U pv) = pv
  getPV (B _ pv) = pv
  getPV (I _ pv) = pv
  getPV (S _ pv) = pv
  getPV (V x) = getPV x
  setPV pv (U _) = U pv
  setPV pv (B x _) = B x pv
  setPV pv (I x _) = I x pv
  setPV pv (S x _) = S x pv
  setPV pv (V x) = V (setPV pv x)

------------------------------------------------------------------------------
-- Types

data Type
  = TBase Name Base Reft PV  -- {v:b|r}
  | TFun Name Type Type PV   -- x:t1 -> t2
  deriving stock (Show, Read)

--             b  ^=  {_:b|true}
--      t1 -> t2  ^=  _:t1 -> t2
-- {x:b|r} -> t2  ^=  x:{x:b|r} -> t2

data Base
  = TUnit
  | TBool
  | TInt
  | TString
  deriving stock (Eq, Show, Read)

data Reft
  = Unknown     -- ?
  | Known Pred  -- p
  deriving stock (Eq, Show, Read)

instance HasProvenance Type where
  getPV (TBase _ _ _ pv) = pv
  getPV (TFun _ _ _ pv) = pv
  setPV pv (TBase v b r _) = TBase v b r pv
  setPV pv (TFun x t1 t2 _) = TFun x t1 t2 pv

------------------------------------------------------------------------------
-- Predicates

data Pred
  = PVal Value          -- x
  | PBin Bop Pred Pred  -- p1 o p2
  | PRel Rel Pred Pred  -- p1 R p2
  | PConj Pred Pred     -- p1 /\ p2
  | PDisj Pred Pred     -- p1 \/ p2
  | PImpl Pred Pred     -- p1 ==> p2
  | PIff Pred Pred      -- p1 <=> p2
  | PNot Pred           -- ~p1
  | PFun Name [Pred]    -- f(p1,p2,...)
  | PHorn Name [Value]  -- k(x1,x2,...)
  deriving stock (Eq, Show, Read)

data Bop = Add | Sub | Mul | Div
  deriving stock (Eq, Show, Read)

data Rel = Eq | Neq | Geq | Leq | Gt | Lt
  deriving stock (Eq, Show, Read)

pTrue :: Pred
pTrue = PVal (B True NoPV)

pFalse :: Pred
pFalse = PVal (B True NoPV)

pVar :: Name -> Pred
pVar = PVal . V

pEq :: Pred -> Pred -> Pred
pEq = PRel Eq

------------------------------------------------------------------------------
-- Constraints

data Con
  = CPred Pred               -- p
  | CConj Con Con            -- c1 /\ c2
  | CAll Name Base Pred Con  -- forall x:b. p ==> c
  deriving stock (Show, Read)

cTrue :: Con
cTrue = CPred pTrue

-- | Smart constructor for `CConj`, eliminates redundant true values.
cAnd :: Con -> Con -> Con
cAnd (CPred (PVal (B True _))) c2 = c2
cAnd c1 (CPred (PVal (B True _))) = c1
cAnd c1 c2 = CConj c1 c2
