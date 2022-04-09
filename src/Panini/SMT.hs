{-# LANGUAGE OverloadedStrings #-}

module Panini.SMT
  ( printSMTLib2
  -- , solve
  ) where

import Data.List (partition)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text.Lazy qualified as LT
import Data.Text.Lazy.Builder (Builder)
import Data.Text.Lazy.Builder qualified as LB
import Panini.Substitution
import Panini.Syntax
import Prelude

------------------------------------------------------------------------------

-- -- | Solve a Horn constraint given a set of candidates.
-- solve :: Con -> [Pred] -> IO Bool
-- solve c q = do
--   let cs = flat c
--   let (csk,csp) = partition horny cs
--   --let s0 = 
--   s <- fixpoint csk s0
--   smtValid (map (applyC s) csp)

-- -- | Flatten a Horn constraint into a set of flat constraints each of which is
-- -- of the form ∀x1:b1. p1 ⇒ ∀x2:b2. p2 ⇒ ... ⇒ p' where p' is either a single
-- -- Horn application κ(ȳ) or a concrete predicate free of Horn variables.
-- flat :: Con -> [Con]
-- flat (CPred p) = [CPred p]
-- flat (CConj p1 p2) = flat p1 ++ flat p2
-- flat (CAll x b p c) = [CAll x b p c' | c' <- flat c]

-- -- | Whether or not a flat constraint has a Horn application in its head.
-- horny :: Con -> Bool
-- horny (CPred p)
--   | PHorn _ _ <- p = True
--   | otherwise      = False
-- horny (CAll _ _ _ c) = horny c
-- horny (CConj _ _) = error "expected flat constraint"

-- getHornVars = undefined

-- smtValid :: [Con] -> IO Bool
-- smtValid = undefined

-- -- | A Horn assignment σ mapping Horn variables κ to predicates over the Horn
-- -- variables parameters x̄.
-- type Assignment = Map Name ([Name], Pred)

-- -- | Apply a Horn assignment σ to a predicate, replacing each Horn application
-- -- κ(ȳ) with its solution σ(κ)[x̄/ȳ].
-- apply :: Assignment -> Pred -> Pred
-- apply s = \case
--   PVal x       -> PVal x
--   PBin o p1 p2 -> PBin o (apply s p1) (apply s p2)
--   PRel r p1 p2 -> PRel r (apply s p1) (apply s p2)
--   PConj p1 p2  -> PConj (apply s p1) (apply s p2)
--   PDisj p1 p2  -> PDisj (apply s p1) (apply s p2)
--   PImpl p1 p2  -> PImpl (apply s p1) (apply s p2)
--   PIff p1 p2   -> PIff (apply s p1) (apply s p2)
--   PNot p       -> PNot (apply s p)
--   PFun f ps    -> PFun f (map (apply s) ps)
--   PHorn k xs   -> case Map.lookup k s of
--     Just (ys,p) -> substN xs ys p
--     Nothing     -> PHorn k xs
--   where
--     substN :: [Value] -> [Name] -> Pred -> Pred
--     substN (x:xs) (y:ys) = substN xs ys . subst x y
--     substN _ _ = id

-- applyC :: Assignment -> Con -> Con
-- applyC s = \case
--   CPred p      -> CPred (apply s p)
--   CConj c1 c2  -> CConj (applyC s c1) (applyC s c2)
--   CAll x b p c -> CAll x b (apply s p) (applyC s c)

------------------------------------------------------------------------------

printSMTLib2 :: SMTLib2 a => a -> Text
printSMTLib2 = LT.toStrict . LB.toLazyText . encode

class SMTLib2 a where
  encode :: a -> Builder

parens :: Builder -> Builder
parens a = "(" <> a <> ")"

(<+>) :: Builder -> Builder -> Builder
(<+>) a b = a <> " " <> b

sexpr :: [Builder] -> Builder
sexpr  = parens . foldr1 (<+>)

instance SMTLib2 Con where
  encode (CPred p) = encode p
  encode (CConj c1 c2) = sexpr ["and", encode c1, encode c2]
  encode (CAll x b p c) = sexpr ["forall", parens sort, impl] 
    where
      sort = sexpr [encode x, encode b]
      impl = sexpr ["=>", encode p, encode c]

instance SMTLib2 Pred where
  encode (PVal v) = encode v
  encode (PBin o p1 p2) = sexpr [encode o, encode p1, encode p2]
  encode (PRel r p1 p2) = sexpr [encode r, encode p1, encode p2]
  encode (PConj p1 p2) = sexpr ["and", encode p1, encode p2]
  encode (PDisj p1 p2) = sexpr ["or", encode p1, encode p2]
  encode (PImpl p1 p2) = sexpr ["=>", encode p1, encode p2]
  encode (PIff p1 p2) = sexpr ["iff", encode p1, encode p2]
  encode (PNot p) = sexpr ["not", encode p]
  encode (PFun x ps) = sexpr (encode x : map encode ps)
  encode (PHorn x vs) = sexpr (encode x : map encode vs)

instance SMTLib2 Name where
  encode (Name n _) = LB.fromText n

instance SMTLib2 Value where
  encode (U _) = "0"  -- TODO
  encode (B True _) = "true"
  encode (B False _) = "false"
  encode (I x _) = LB.fromString (show x)
  encode (S x _) = LB.fromString (show x)
  encode (V x) = encode x

instance SMTLib2 Bop where
  encode Add = "+"
  encode Sub = "-"
  encode Mul = "*"
  encode Div = "/"

instance SMTLib2 Rel where
  encode Eq = "="
  encode Neq = "disequal"
  encode Geq = ">="
  encode Leq = "<="
  encode Gt = ">"
  encode Lt = "<"

instance SMTLib2 Base where
  encode TUnit = "Int"  -- TODO
  encode TBool = "Bool"
  encode TInt = "Int"
  encode TString = "String"

