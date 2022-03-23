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
data Name = Name Text
  deriving stock (Eq, Ord, Show, Read)

instance IsString Name where
  fromString = Name . Text.pack

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
nextSubscript (Name n) = case decimal @Int $ Text.takeWhileEnd isDigit n of
  Left _      -> Name $ Text.append n "1"
  Right (i,_) -> Name $ Text.append n $ Text.pack $ show (i + 1)

------------------------------------------------------------------------------
-- Top-level declarations

type Prog = [Decl]

data Decl
  = Assume PV Name Type   -- assume x : t
  | Define PV Name Term   -- define x = e
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
  = U          -- unit
  | B Bool     -- true, false
  | I Integer  -- 0, -1, 1, ...
  | S Text     -- "lorem ipsum"
  | V Name     -- x
  deriving stock (Eq, Ord, Show, Read)

------------------------------------------------------------------------------
-- Types

data Type
  = TBase Name Base Reft  -- {v:b|r}
  | TFun Name Type Type   -- x:t1 -> t2
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

simpleType :: Base -> Type
simpleType b = TBase dummyName b (Known pTrue)

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
  deriving stock (Eq, Show, Read)

data Bop = Add | Sub | Mul | Div
  deriving stock (Eq, Show, Read)

data Rel = Eq | Neq | Geq | Leq | Gt | Lt
  deriving stock (Eq, Show, Read)

pTrue :: Pred
pTrue = PVal (B True)

pFalse :: Pred
pFalse = PVal (B True)

pVar :: Name -> Pred
pVar = PVal . V

------------------------------------------------------------------------------
-- Constraints

data Con
  = CPred Pred               -- p
  | CConj Con Con            -- c1 /\ c2
  | CAll Name Base Pred Con  -- forall x:b. p ==> c
  deriving stock (Show, Read)

cTrue :: Con
cTrue = CPred pTrue
