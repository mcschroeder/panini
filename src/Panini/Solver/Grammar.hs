{-# LANGUAGE OverloadedStrings #-}

module Panini.Solver.Grammar (solve) where

-- import Debug.Trace
-- import Data.Bifunctor
-- import Data.Foldable
-- import Data.Map.Strict (Map)
-- import Data.Map.Strict qualified as Map
import Panini.Printer
import Panini.Syntax
import Prelude
import Panini.Solver.AbstractInteger qualified as A
import Panini.Pretty.Graphviz
import Prettyprinter (vsep)

data GTree
  = GAnd GTree GTree
  | GAll Name Base GTree
  | GImpl GTree GTree
  | GOr [GExpr] [GExpr]
  | GExpr [GExpr]
  deriving stock (Show, Read)

data GExpr 
  = GAtom Bool Name
  | GVarIntAbs Name A.AbstractInteger
  | GPred Pred -- TODO: remove
  deriving stock (Show, Read)

instance GraphViz GTree where
  dot = fromDAG . dag
    where
      dag (GAnd t1 t2)          = Node Circle "∧" [dag t1, dag t2]
      dag (GAll (Name x _) _ t) = Node Circle ("∀ " <> x) [dag t]
      dag (GImpl t1 t2)         = Node Circle "⇒" [dag t1, dag t2]
      dag (GOr xs ys)           = Node Circle "∨" [dagE xs, dagE ys]
      dag (GExpr xs)            = dagE xs
      dagE xs                   = Node Box lbl []
        where
          opts = RenderOptions False True Nothing
          lbl = renderDoc opts $ vsep $ map pretty xs

instance Pretty GExpr where
  pretty (GAtom True x) = pretty x
  pretty (GAtom False x) = "¬" <> pretty x
  pretty (GVarIntAbs x a) = pretty x <> " = " <> pretty a
  pretty (GPred p) = pretty p

solve :: Con -> Pred
solve c = 
  let t = traceGraph "trace.png" $ solve' c
  in t `seq` PFalse NoPV

solve' :: Con -> GTree
solve' = treeify

treeify :: Con -> GTree
treeify = goC
  where
    goC (CAnd c1 c2)   = GAnd (goC c1) (goC c2)
    goC (CAll x b p c) = GAll x b (GImpl (goP p) (goC c))
    goC (CHead p)      = goP p

    goP       (PVar x)  = GExpr [GAtom True  x]
    goP (PNot (PVar x)) = GExpr [GAtom False x]
    
    goP (PAnd (p:[])) = goP p
    goP (PAnd (p:ps)) = GAnd (goP p) (goP (PAnd ps))
    
    goP (PRel r (PVar x) (PCon (I i _))) = GExpr [GVarIntAbs x (relToAbsInt r i)]
    
    goP p = GExpr [GPred p]


relToAbsInt :: Rel -> Integer -> A.AbstractInteger
relToAbsInt r i = case r of
  Eq  -> A.mkEq i
  Neq -> A.mkNeq i
  Gt  -> A.mkGt i
  Geq -> A.mkGeq i
  Lt  -> A.mkLt i
  Leq -> A.mkLeq i

{-


-- solve :: Con -> Pred
-- solve = rewrite' resolveIffs . norm . elim

-------------------------------------------------------------------------------

-- | Eliminate quantifiers.
elim :: Con -> Pred
elim (CIf p1 c1 p2 c2) = (p1 `pAnd` elim c1) `pOr` (p2 `pAnd` elim c2)
elim (CAnd c1 c2)      = elim c1 `pAnd` elim c2
elim (CAll _ _ p c)    = p `pAnd` elim c
elim (CHead p)         = p

pattern CIf :: Pred -> Con -> Pred -> Con -> Con
pattern CIf p1 c1 p2 c2 <- 
  CAnd (CAll _ TUnit p1@(PVar x) c1) 
       (CAll _ TUnit p2@(PNot (PVar ((== x) -> True))) c2)

-------------------------------------------------------------------------------

-- | Rewrite predicate into normal form.
norm :: Pred -> Pred
norm (PNot p)
  | PRel r x y    <- p = norm (PRel (invRel r) x y)
  | PCon (B b pv) <- p = PCon (B (not b) pv)
    
norm (PRel r x y)
  | PCon _ <- x, PVar _   <- y = PRel (convRel r) y x
  | PCon _ <- x, PFun _ _ <- y = PRel (convRel r) y x

norm (PAnd ps) = PAnd $ map norm ps
norm (POr  ps) = POr  $ map norm ps

norm p = p

-- | Inverse of a relation, e.g., ≥ to <.
invRel :: Rel -> Rel
invRel = \case
  Eq  -> Neq
  Neq -> Eq
  Geq -> Lt
  Leq -> Gt
  Gt  -> Leq
  Lt  -> Geq

-- | Converse of a relation, e.g., ≥ to ≤.
convRel :: Rel -> Rel
convRel = \case
  Eq  -> Eq
  Neq -> Neq
  Geq -> Leq
  Leq -> Geq
  Gt  -> Lt
  Lt  -> Gt

-------------------------------------------------------------------------------

-- | Rewrite a predicate using knowledge collected along the way.
--
-- Conjunctions ('PAnd') are rewritten sequentially, with knowledge flowing left
-- to right. Disjunctions ('POr') are rewritten independently of one another,
-- with knowledge only flowing downwards, but not across the alternatives.
rewrite :: (k -> Pred -> (k, Pred)) -> k -> Pred -> (k, Pred)
rewrite f = go
  where
    go k (PAnd ps) = foldl' (\(k', q) -> second (pAnd q) . go k') (k, tt) ps
    go k (POr  ps) = (k, POr $ map (snd . go k) ps)
    go k p         = f k p

rewrite' :: Monoid k => (k -> Pred -> (k, Pred)) -> Pred -> Pred
rewrite' f = snd . rewrite f mempty

tt :: Pred
tt = PTrue NoPV

-------------------------------------------------------------------------------

resolveIffs :: Map Name Pred -> Pred -> (Map Name Pred, Pred)
resolveIffs k p = case p of
  PIff (PVar x) q -> (Map.insert x q k, tt)
  
  PVar x -> case Map.lookup x k of
    Just q        -> (k, q)
    Nothing       -> (k, p)    
  
  PNot (PVar x) -> case Map.lookup x k of
    Just q        -> (k, norm (PNot q))
    Nothing       -> (k, p)
  
  _               -> (k, p)
    
-------------------------------------------------------------------------------

-}