{-# LANGUAGE OverloadedStrings #-}

module Language.Panini.Syntax where

import Data.Char
import Data.String
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Read

------------------------------------------------------------------------------
-- Names

newtype Name = Name Text
  deriving (Eq, Ord, Show, Read)

instance IsString Name where
  fromString = Name . Text.pack

dummyName :: Name
dummyName = Name "_"

isDummy :: Name -> Bool
isDummy (Name "_") = True
isDummy _          = False

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
  Right (i,_) -> Name $ Text.append n $ Text.pack $ show (i+1)

------------------------------------------------------------------------------
-- Values

data Value
  = U          -- unit
  | B Bool     -- true, false
  | I Integer  -- 0, -1, 1, ...
  | S Text     -- "lorem ipsum"
  | V Name     -- x
  deriving (Eq, Ord, Show, Read)

------------------------------------------------------------------------------
-- Terms

data Expr
  = Val Value                -- x
  | App Expr Value           -- e x
  | Lam Name Expr            -- \x. e
  | Ann Expr Type            -- e : t
  | Let Name Expr Expr       -- let x = e1 in e2  
  | Rec Name Type Expr Expr  -- rec x : t = e1 in e2
  | If Value Expr Expr       -- if x then e1 else e2    
  deriving (Show, Read)

------------------------------------------------------------------------------
-- Types

data Type
  = TBase Name Base Reft  -- {v:b|r}
  | TFun Name Type Type   -- x:t1 -> t2
  deriving (Show, Read)

--             b  ^=  {_:b|true}
--      t1 -> t2  ^=  _:t1 -> t2
-- {x:b|r} -> t2  ^=  x:{x:b|r} -> t2

data Base
  = TUnit
  | TBool
  | TInt
  | TString
  deriving (Eq, Show, Read)

data Reft
  = Unknown     -- ?
  | Known Pred  -- p
  deriving (Eq, Show, Read)

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
  deriving (Eq, Show, Read)

data Bop = Add | Sub | Mul | Div
  deriving (Eq, Show, Read)

data Rel = Eq | Neq | Geq | Leq | Gt | Lt
  deriving (Eq, Show, Read)

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
  deriving (Show, Read)

cTrue :: Con
cTrue = CPred pTrue
