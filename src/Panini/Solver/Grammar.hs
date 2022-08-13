{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Panini.Solver.Grammar (solve) where

import Algebra.Lattice
import Panini.Printer
import Panini.Syntax
import Prelude
import Panini.Solver.AInteger qualified as A
import Panini.Pretty.Graphviz
import Data.List qualified as List
import Data.Text qualified as Text
import Debug.Trace
import Data.Bifunctor
import Data.Foldable

-------------------------------------------------------------------------------

solve :: Con -> Pred
solve c = 
  let t = traceGraph "trace.png" $ solve' c
  in t `seq` PFalse NoPV

solve' :: Con -> GTree
solve' = GExpr . resolve . treeify

-------------------------------------------------------------------------------

data GTree
  = GAnd GTree GTree
  | GAll Name Base GTree
  | GImpl GTree GTree
  | GExpr GExpr
  deriving stock (Show, Read)

data GExpr
  = GSimple [GFact]
  | GChoice [GFact] GExpr
  deriving stock (Show, Read)

data GFact
  = GAtom Bool Name
  | GIff Name Pred
  | GVarIntAbs Name A.AInteger
  | GPred Pred -- TODO: remove  
  deriving stock (Show, Read)

-------------------------------------------------------------------------------

instance GraphViz GTree where
  dot = fromDAG . dag
    where
      dag (GAnd t1 t2)          = CircleNode "∧" [dag t1, dag t2]
      dag (GAll (Name x _) _ t) = Node [Shape Diamond, Label ("∀ " <> x)] [dag t]
      dag (GImpl t1 t2)         = CircleNode "⇒" [dag t1, dag t2]
      dag (GExpr x)             = dagE x
      
      dagE e = Node [Shape Record, Label (lab e)] []
      lab (GSimple xs) = labS xs
      lab (GChoice xs e) = labS xs <> "|" <> lab e
      labS [x] = rend $ pretty x
      labS xs = "{" <> rend (mconcat $ map ((<> "\\l") . pretty) xs) <> "}"
      opts = RenderOptions False True Nothing
      rend = Text.pack . esc . Text.unpack . renderDoc opts
      esc (x:xs) 
        | x `elem` ("|{}<>" :: String) = '\\':x:esc xs
        | otherwise = x:esc xs
      esc [] = []

instance Pretty GFact where
  pretty (GAtom True x)   = pretty x
  pretty (GAtom False x)  = "¬" <> pretty x
  pretty (GIff x p)       = pretty x <> " ⟺ " <> pretty p
  pretty (GVarIntAbs x a) = pretty x <> " = " <> pretty a
  pretty (GPred p)        = pretty p

-------------------------------------------------------------------------------

treeify :: Con -> GTree
treeify = goC
  where
    goC (CAnd c1 c2)   = GAnd (goC c1) (goC c2)
    goC (CAll x b p c) = GAll x b (GImpl (goP p) (goC c))
    goC (CHead p)      = goP p
    goP (PAnd [p])     = goP p
    goP (PAnd (p:ps))  = GAnd (goP p) (goP (PAnd ps))
    goP p              = GExpr $ GSimple [predToFact p]

predToFact :: Pred -> GFact
predToFact = \case
  PNot (PCon (B b pv))           -> predToFact $ PCon (B (not b) pv)  
  PNot (PRel r x y)              -> predToFact $ PRel (invRel r) x y
  PRel r x@(PCon _) y@(PVar _)   -> predToFact $ PRel (convRel r) y x
  PRel r x@(PCon _) y@(PFun _ _) -> predToFact $ PRel (convRel r) y x

  PRel r (PFun "len" [PVar "s"]) y -> predToFact $ PRel r (PVar "|s|") y
  PRel r x (PFun "len" [PVar "s"]) -> predToFact $ PRel r x (PVar "|s|")

  PVar x                         -> GAtom True  x
  PNot (PVar x)                  -> GAtom False x
  PRel r (PVar x) (PCon (I i _)) -> GVarIntAbs x (relToAbsInt r i)
  PIff (PVar x) q                -> GIff x q
  p                              -> GPred p

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

relToAbsInt :: Rel -> Integer -> A.AInteger
relToAbsInt r i = case r of
  Eq  -> A.mkEq i
  Neq -> A.mkNeq i
  Gt  -> A.mkGt i
  Geq -> A.mkGeq i
  Lt  -> A.mkLt i
  Leq -> A.mkLeq i

-------------------------------------------------------------------------------

resolve :: GTree -> GExpr
resolve (GExpr e) = e
resolve (GImpl t1 t2) = resolve t1 `meetExpr` resolve t2
resolve (GAll x _ t) = solveFor x (resolve t)
resolve (GAnd t1 t2) =
  let e1 = resolve t1
      e2 = resolve t2
  in case e1 `meetExpr` e2 of
    GSimple [] -> e1 `orExpr` e2
    z -> z

meetExpr :: GExpr -> GExpr -> GExpr
meetExpr (GSimple xs) (GSimple ys) = GSimple $ meetFacts (xs ++ ys)
meetExpr e1@(GSimple xs) (GChoice ys e2) = GChoice (meetFacts (xs ++ ys)) (e1 `meetExpr` e2)
meetExpr (GChoice xs e1) e2@(GSimple ys) = GChoice (meetFacts (xs ++ ys)) (e1 `meetExpr` e2)
--meetExpr (GChoice xs e1) (GChoice ys e2) = undefined

orExpr :: GExpr -> GExpr -> GExpr
orExpr (GSimple xs) e = GChoice xs e
orExpr (GChoice xs e1) e2 = GChoice xs (e1 `orExpr` e2)

meetFacts :: [GFact] -> [GFact]
meetFacts [] = []
meetFacts (x0:xs0) = go x0 [] xs0
  where
    go z    []     []  = z : []
    go z (y:ys)    []  = z : go y [] ys
    go z    ys  (x:xs) = case meetFact z x of
      Top     -> go z (x:ys) xs
      Meet_ z' -> go z'   ys  xs
      Bottom  -> []


data Meet_ a = Top | Meet_ a | Bottom

meetFact :: GFact -> GFact -> Meet_ GFact
meetFact f1 f2 = case (f1, f2) of
  (GAtom pa a, GAtom pb b) 
    | a == b, pa /= pb -> Bottom
    | a == b, pa == pb -> Meet_ $ GAtom pa a

  (GIff _ _, GAtom _ _) -> meetFact f2 f1
  (GAtom pa a, GIff b p)
    | a == b, pa == True  -> Meet_ $ predToFact p
    | a == b, pa == False -> Meet_ $ predToFact (PNot p)

  (GVarIntAbs a i1, GVarIntAbs b i2)
    | a == b -> Meet_ $ GVarIntAbs a (i1 /\ i2)

  _ -> Top

solveFor :: Name -> GExpr -> GExpr
solveFor _ = id

-- resolve :: GTree -> GExpr
-- resolve (GExpr xs) = xs
-- resolve (GAnd t1 t2) = 
--   let x = resolve t1 
--       y = resolve t2
--   in case x `meetExpr` y of
--     GFacts [] -> GChoice x y
--     z         -> z
-- resolve (GImpl t1 t2) = resolve t1 `meetExpr` resolve t2
-- resolve (GAll x _ t) = solveFor x $ resolve t

-- solveFor :: Name -> GExpr -> GExpr
-- solveFor _ = id

-- meetExpr :: GExpr -> GExpr -> GExpr
-- meetExpr (GFacts xs) (GFacts ys) = undefined
-- meetExpr (GChoice xs x2) (GFacts ys) = undefined
-- meetExpr (GFacts xs) (GChoice ys y2) = undefined
-- meetExpr (GChoice xs x2) (GChoice ys y2) = undefined


-- resolve (GOr xs ys) = GOr xs ys
-- resolve (GImpl (resolve -> GExpr xs) (resolve -> GExpr ys)) = GExpr (xs ++ ys)
-- resolve (GImpl (resolve -> GExpr ys) (resolve -> GOr ys zs)) = GExpr
--   (GExpr xs, GOr ys zs) -> GOr (xs ++ ys) (xs ++ zs)  -- TODO: meet
--   (GOr xs ys, GExpr zs) -> GOr (xs ++ zs) (ys ++ zs) -- TODO: meet
--   (t1', t2') -> GImpl t1' t2'


-- resolve (GImpl t1 t2) = case (resolve t1, resolve t2) of
--   (GExpr xs, GExpr ys) -> GExpr (xs ++ ys)  -- TODO: meet
--   (GExpr xs, GOr ys zs) -> GOr (xs ++ ys) (xs ++ zs)  -- TODO: meet
--   (GOr xs ys, GExpr zs) -> GOr (xs ++ zs) (ys ++ zs) -- TODO: meet
--   (t1', t2') -> GImpl t1' t2'
-- resolve (GAll _ _ p) = resolve p  -- TODO: solve for x
-- resolve (GAnd t1 t2) = case (resolve t1, resolve t2) of
--   (GExpr xs, GExpr ys) -> GExpr (xs ++ ys) -- TODO: meet
--   (GExpr xs, GOr ys zs) -> GOr (xs ++ ys) (xs ++ zs)  -- TODO: meet
--   (GOr xs ys, GExpr zs) -> GOr (xs ++ zs) (ys ++ zs) -- TODO: meet
--   (t1', t2') -> GImpl t1' t2'




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