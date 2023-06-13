module Panini.Solver.Grammar2 (infer) where

import Panini.Syntax
import Panini.Algebra.Lattice
import Prelude
import Data.List (foldl')
import Panini.Solver.Grammar2.Tree
import Panini.Solver.Grammar2.Abstract
import Panini.Solver.Grammar2.Rewriting
import Panini.Solver.Abstract.AString

import Panini.Monad
import Panini.Logger
import Panini.Pretty.Printer
import Data.Foldable
import Debug.Trace

import Data.HashSet qualified as HashSet

-------------------------------------------------------------------------------

-- infer2 :: Name -> Con -> Pan Pred
-- infer2 s c = do
--   let !t1 = construct c
--   let !t2 = rewrite t1
--   let !ps = toPreds t2
--   let !a1 = map (foldl' (∧) (⊤) . map (abstractStringVar s)) ps
--   let !a2 = foldl' (∨) (⊥) a1
--   let !t3 = concretizeVar s $ TAbs $ AString a2
--   let !c2 = destruct t3
--   return c2

-- | Algorithm 2 in OOPSLA'23 submission.
infer :: Name -> Con -> Pred
--infer s c = 
  -- let t = construct c in
  -- let t' = rewrite t in
  -- trace (showPretty t') $ destruct t'
infer s = --destruct 
          concretizeVar s . PAbs . AString
        . foldl' (∨) (⊥) 
        . map (foldl' (∧) (⊤) . map (abstractStringVar s))  -- TODO
        . toPredsDNF
        . rewrite
        -- . TAll s TString
        -- . construct

-- TODO: either make this unnecessary or deal with errors gracefully
abstractStringVar :: Name -> Pred -> AString
abstractStringVar x p = case abstractVar x TString p of
  PAbs (AString s) -> s
  _                -> error "expected abstract string"

-------------------------------------------------------------------------------

-- TODO: ensure PRel at bottom
toPredsDNF :: Pred -> [[Pred]]
toPredsDNF (POr ps) | all isPAnd ps = [xs | PAnd xs <- ps]
toPredsDNF (PAnd xs) | all isPRel xs = [xs]
toPredsDNF c = error $ "expected (POr [PAnd _]) instead of " ++ showPretty c

toPredsCon :: Con -> [[Pred]]
toPredsCon (CHead (POr ps)) | all isPAnd ps = [xs | PAnd xs <- ps]
toPredsCon c = error $ "expected CHead (POr [PAnd _]) instead of " ++ showPretty c


construct :: Con -> Tree
construct = goC
  where
    goC (CAnd c1 c2)   = TAnd $ HashSet.fromList [goC c1, goC c2]
    goC (CAll x b p c) = TAll x b (TImp (goP p) (goC c))
    goC (CHead p)      = goP p
    goP (PAnd [p])     = goP p
    goP (PAnd (p:ps))  = TAnd $ HashSet.fromList [goP p, goP (PAnd ps)]
    goP (PIff p q)     = TIff (goP p) (goP q)
    goP (PNot p)       = TNeg (goP p)
    goP PTrue          = TTrue
    goP PFalse         = TFalse
    goP (PRel r e1 e2) = TPred $ TRel r (goE e1) (goE e2)

    goP (PAppK _ _)    = TTrue  -- TODO: THIS IS JUST A HACK FOR PLAYING AROUND!!!
    
    goP p              = error $ "not implemented: construct " ++ showPretty p
    
    goE (PVar x)       = TVar x
    goE (PCon c)       = TCon c
    goE (PStrLen e)    = TStrLen (goE e)
    goE (PStrAt (PVar s) (PCon i)) = TStrAt (TVar s) (TCon i)  -- TODO
    goE e              = error $ "not implemented: construct " ++ showPretty e

destruct :: Tree -> Pred
destruct = goT
  where
    goT (TOr ts) = foldr (∨) PFalse $ map goT $ toList ts
    goT (TAnd ts) = foldr (∧) PTrue $ map goT $ toList ts
    -- goT (TOr t1 t2) = foldr (∨) PFalse $ map goT [t1,t2]
    -- goT (TAnd t1 t2) = foldr (∧) PTrue $ map goT [t1,t2]
    goT (TPred (TRel r e1 e2))   = PRel r (goE e1) (goE e2)
    goT (TPred (TReg (TVal v) re)) = PReg v (showPretty re) -- TODO
    goT TTrue       = PTrue
    goT TFalse      = PFalse
    goT t           = error $ "not implemented: destruct " ++ showPretty t

    goE (TVar x)     = PVar x
    goE (TCon c)     = PCon c
    goE (TStrLen e)  = PStrLen (goE e)
    goE (TStrAt (TVar s) (TCon (I i _))) = PStrAt (PVar s) (PCon (I i NoPV))
    goE e            = error $ "not implemented: destruct " ++ showPretty e
