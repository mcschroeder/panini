{-# LANGUAGE OverloadedStrings #-}

module Language.Panini.Syntax where

import Data.String
import Data.Text (Text)
import Data.Text qualified as Text

------------------------------------------------------------------------------
-- Names

newtype Name = Name Text
  deriving (Eq, Show, Read)

instance IsString Name where
  fromString = Name . Text.pack

dummyName :: Name
dummyName = Name "_"

isDummy :: Name -> Bool
isDummy (Name "_") = True
isDummy _          = False

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

data Value
  = U          -- unit
  | B Bool     -- true, false
  | I Integer  -- 0, -1, 1, ...
  | S Text     -- "lorem ipsum"
  | V Name     -- x
  deriving (Show, Read)

------------------------------------------------------------------------------
-- Types

data Type
  = Base Name BaseType Refinement  -- {x : t | r}
  | Pi Name Type Type              -- x : t1 -> t2
  deriving (Show, Read)

--                    t  ^=  {_ : t | true}
--             t1 -> t2  ^=  _ : t1 -> t2
-- { x : t1 | r } -> t2  ^=  x : {x : t1 | r} -> t2

data BaseType
  = TyUnit
  | TyBool
  | TyInt
  | TyString
  deriving (Eq, Show, Read)

data Refinement 
  = Unknown           -- ?
  | Known Pred        -- p
  deriving (Eq, Show, Read)

------------------------------------------------------------------------------
-- Predicates

data Pred
  = PTrue               -- true
  | PFalse              -- false
  | PVar Name           -- x
  | PInt Integer        -- c
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

------------------------------------------------------------------------------
-- Constraints

data Con
  = CPred Pred                   -- p
  | CConj Con Con                -- c1 /\ c2
  | CAll Name BaseType Pred Con  -- forall x:b. p ==> c
  deriving (Show, Read)
