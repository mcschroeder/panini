{-# LANGUAGE OverloadedStrings #-}

module Language.Panini.Syntax where

import Data.Text (Text)

------------------------------------------------------------------------------
-- Names

newtype Name = Name Text
  deriving (Eq, Show, Read)

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
  = Unit
  | B Bool
  | I Integer
  | S Text
  | Var Name
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
  deriving (Show, Read)

data Refinement 
  = Unknown           -- ?
  | Known Pred
  deriving (Eq, Show, Read)

------------------------------------------------------------------------------
-- Predicates

data Pred
  = PTrue              -- true
  | PFalse             -- false
  | PVar Name          -- x
  | PInt Integer       -- c
  | PConj Pred Pred    -- p1 & p2
  | PDisj Pred Pred    -- p1 | p2
  | PNeg Pred          -- ~p1
  | POp POp Pred Pred  -- p1 o p2       (interpreted operator)
  | PUf Name [Pred]    -- f(p1,p2,...)  (uninterpreted function)
  deriving (Eq, Show, Read)

data POp
  = Eq   -- =
  | Neq  -- /=
  | Leq  -- <=
  | Geq  -- >=
  | Lt   -- <
  | Gt   -- >
  | Add  -- +
  | Sub  -- -
  | Mul  -- *
  | Div  -- /
  deriving (Eq, Show, Read)
