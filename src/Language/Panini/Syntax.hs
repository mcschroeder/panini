{-# LANGUAGE OverloadedStrings #-}

module Language.Panini.Syntax where

import Data.Text (Text)

------------------------------------------------------------------------------
-- Names

newtype Name = Name Text
  deriving (Show, Read)

dummyName :: Name
dummyName = Name "_"

isDummy :: Name -> Bool
isDummy (Name "_") = True
isDummy _          = False

------------------------------------------------------------------------------
-- Terms

data Expr
  = Val Value                -- ^ x
  | App Expr Value           -- ^ e x
  | Lam Name Expr            -- ^ \x. e
  | Ann Expr Type            -- ^ e : t
  | Let Name Expr Expr       -- ^ let x = e1 in e2  
  | Rec Name Type Expr Expr  -- ^ rec x : t = e1 in e2
  | If Value Expr Expr       -- ^ if x then e1 else e2    
  deriving (Show, Read)

data Value
  = Var Name
  | Con Constant
  deriving (Show, Read)

data Constant
  = Unit
  | B Bool
  | I Integer
  | S Text
  deriving (Show, Read)

------------------------------------------------------------------------------
-- Types

data Type
  = Base Name BaseType Refinement  -- ^ {x : t | r}
  | Pi Name Type Type              -- ^ x : t1 -> t2
  deriving (Show, Read)

--                    t  ^=  {_ : t | true}
--             t1 -> t2  ^=  (_ : t1) -> t2
-- { x : t1 | r } -> t2  ^=  (x : {x : t1 | r}) -> t2

data BaseType
  = TyUnit
  | TyBool
  | TyInt
  | TyString
  deriving (Show, Read)

data Refinement 
  = Unknown           -- ^ ?
  | Known Pred
  deriving (Show, Read)

------------------------------------------------------------------------------
-- Predicates

data Pred
  = PVar Name                 -- ^ x,y,z,...
  | PBool Bool                -- ^ true, false
  | PInt Integer              -- ^ 0,-1,1,...
  | PIntOp IntOp Pred Pred    -- ^ p1 + p2, p1 - p2, ...
  | PConj Pred Pred           -- ^ p1 /\ p2
  | PDisj Pred Pred           -- ^ p1 \/ p2
  | PNeg Pred                 -- ^ -p1
  | PFunc Name [Pred]         -- ^ f(p1,p2,...)
  deriving (Show, Read)

data IntOp = Plus | Minus | Times | Div
  deriving (Show, Read)
