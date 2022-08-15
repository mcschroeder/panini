{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Panini.Solver.Grammar (solve) where

import Data.Text qualified as Text
import Panini.Pretty.Graphviz
import Panini.Printer
import Panini.Solver.Abstract.ABool
import Panini.Solver.Abstract.AChar
import Panini.Solver.Abstract.AInteger
import Panini.Solver.Abstract.Lattice
import Panini.Syntax
import Prelude

-------------------------------------------------------------------------------

solve :: Con -> Pred
solve c = 
  let t = traceGraph "trace.png" $ solve' c
  in t `seq` PFalse NoPV

solve' :: Con -> Tree
solve' = treeify

-------------------------------------------------------------------------------

data Tree
  = TOr Tree Tree   -- p ∨ q
  | TAnd Tree Tree  -- p ∧ q
  | TTerm APredSet
  deriving stock (Show, Read)

type APredSet = [APred]

data APred
  = AEq Name AExpr           -- ^ x = α
  | AStrAt Name AExpr AExpr  -- ^ x[α] = β
  | AStrLen Name AExpr       -- ^ |x| = α
  | AUnknown Pred
  deriving stock (Eq, Show, Read)

data AExpr
  = AInteger AInteger
  | ABool ABool
  | AChar AChar
  | AVar Name
  deriving stock (Eq, Show, Read)

-------------------------------------------------------------------------------

instance GraphViz Tree where
  dot = fromDAG . dag
    where
      dag (TOr   p q) = CircleNode "∨" [dag p, dag q]
      dag (TAnd  p q) = CircleNode "∧" [dag p, dag q]
      dag (TTerm fs)  = BoxNode (termLabel fs) []      
      termLabel [x] = rend $ pretty x
      termLabel xs  = rend $ mconcat $ map ((<> "\\l") . pretty) xs
      rend = renderDoc (RenderOptions False True Nothing)

instance Pretty APred where
  pretty (AEq x a) = pretty x <> " = " <> pretty a
  pretty (AStrAt x a b) = pretty x <> "[" <> pretty a <> "] = " <> pretty b
  pretty (AStrLen x a) = "|" <> pretty x <> "| = " <> pretty a
  pretty (AUnknown p) = "⟨ " <> pretty p <> " ⟩"

instance Pretty AExpr where
  pretty (AInteger a) = pretty a
  pretty (ABool a) = pretty a
  pretty (AChar a) = pretty a
  pretty (AVar x) = pretty x

-------------------------------------------------------------------------------

treeify :: Con -> Tree
treeify = goC
  where
    goC (CAnd c1 c2)       = TAnd (goC c1) (goC c2)    
    goC (CAll x TInt  p c) = TAnd (TTerm [AEq x (AInteger (⊤))]) (TAnd (goP p) (goC c))
    goC (CAll x TBool p c) = TAnd (TTerm [AEq x (ABool (⊤))]) (TAnd (goP p) (goC c))    
    goC (CAll _ _     p c) = TAnd (goP p) (goC c)
    goC (CHead p)          = goP p    
    goP (PAnd [p])         = goP p
    goP (PAnd (p:ps))      = TAnd (goP p) (goP (PAnd ps))
    goP (PIff p q)         = TOr (TAnd (goP p) (goP q)) (TAnd (goP (PNot p)) (goP (PNot q)))
    goP p                  = TTerm [factify p]

factify :: Pred -> APred
factify = \case
  PNot (PCon (B b pv)) -> factify $ PCon (B (not b) pv)  
  PNot (PRel r x y)    -> factify $ PRel (invRel r) x y

  PVar x        -> AEq x $ ABool $ aBoolEq True
  PNot (PVar x) -> AEq x $ ABool $ aBoolEq False
  
  PRel r y@(PCon _) x@(PVar _) -> factify $ PRel (convRel r) x y
  PRel r   (PVar x) (PCon (I i _)) -> AEq x $ AInteger $ aIntegerRel r i
  PRel Eq  (PVar x) (PCon (B b _)) -> AEq x $ ABool $ aBoolEq b
  PRel Neq (PVar x) (PCon (B b _)) -> AEq x $ ABool $ aBoolEq (not b)
  PRel Eq  (PVar x) (PConChar c)   -> AEq x $ AChar $ aCharEq c
  PRel Neq (PVar x) (PConChar c)   -> AEq x $ AChar $ aCharNeq c
  
  PRel Eq (PVar x) (PVar y) -> AEq x (AVar y)

  PRel r y x@(PStrLen _) -> factify $ PRel (convRel r) x y
  PRel r  (PStrLen s) (PCon (I i _)) -> AStrLen s $ AInteger $ aIntegerRel r i
  PRel Eq (PStrLen s) (PVar x)       -> AStrLen s $ AVar x

  PRel r y x@(PStrAt _ _) -> factify $ PRel (convRel r) x y  
  PRel Eq  (PStrAt s (PCon (I i _))) (PConChar c) -> AStrAt s (AInteger $ aIntegerEq i) (AChar $ aCharEq c)
  PRel Neq (PStrAt s (PCon (I i _))) (PConChar c) -> AStrAt s (AInteger $ aIntegerEq i) (AChar $ aCharNeq c)
  PRel Eq  (PStrAt s (PCon (I i _))) (PVar y)     -> AStrAt s (AInteger $ aIntegerEq i) (AVar y)  
  PRel Eq  (PStrAt s (PVar x))       (PConChar c) -> AStrAt s (AVar x) (AChar $ aCharEq c)
  PRel Neq (PStrAt s (PVar x))       (PConChar c) -> AStrAt s (AVar x) (AChar $ aCharNeq c)
  PRel Eq  (PStrAt s (PVar x))       (PVar y)     -> AStrAt s (AVar x) (AVar y)

  p -> AUnknown p

pattern PConChar :: Char -> Pred
pattern PConChar c <- PCon (S (Text.unpack -> [c]) _)

pattern PStrLen :: Name -> Pred
pattern PStrLen x <- PFun "len" [PVar x]

pattern PStrAt :: Name -> Pred -> Pred
pattern PStrAt x p <- PFun "charat" [PVar x, p]

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

aIntegerRel :: Rel -> Integer -> AInteger
aIntegerRel r i = case r of
  Eq  -> aIntegerEq i
  Neq -> aIntegerNeq i
  Gt  -> aIntegerGt i
  Geq -> aIntegerGeq i
  Lt  -> aIntegerLt i
  Leq -> aIntegerLeq i

-------------------------------------------------------------------------------

-- resolve :: Tree -> GExpr
-- resolve (GExpr e) = e
-- resolve (TImpl t1 t2) = resolve t1 `meetExpr` resolve t2
-- resolve (TAll x _ t) = solveFor x (resolve t)
-- resolve (TAnd t1 t2) =
--   let e1 = resolve t1
--       e2 = resolve t2
--   in case e1 `meetExpr` e2 of
--     GSimple [] -> e1 `orExpr` e2
--     z -> z

-- meetExpr :: GExpr -> GExpr -> GExpr
-- meetExpr (GSimple xs) (GSimple ys) = GSimple $ meetFacts (xs ++ ys)
-- meetExpr e1@(GSimple xs) (GChoice ys e2) = GChoice (meetFacts (xs ++ ys)) (e1 `meetExpr` e2)
-- meetExpr (GChoice xs e1) e2@(GSimple ys) = GChoice (meetFacts (xs ++ ys)) (e1 `meetExpr` e2)
-- --meetExpr (GChoice xs e1) (GChoice ys e2) = undefined

-- orExpr :: GExpr -> GExpr -> GExpr
-- orExpr (GSimple xs) e = GChoice xs e
-- orExpr (GChoice xs e1) e2 = GChoice xs (e1 `orExpr` e2)

-- meetFacts :: [GFact] -> [GFact]
-- meetFacts [] = []
-- meetFacts (x0:xs0) = go x0 [] xs0
--   where
--     go z    []     []  = z : []
--     go z (y:ys)    []  = z : go y [] ys
--     go z    ys  (x:xs) = case meetFact z x of
--       Top     -> go z (x:ys) xs
--       Meet_ z' -> go z'   ys  xs
--       Bottom  -> []


-- data Meet_ a = Top | Meet_ a | Bottom

-- meetFact :: GFact -> GFact -> Meet_ GFact
-- meetFact f1 f2 = case (f1, f2) of
--   (GAtom pa a, GAtom pb b) 
--     | a == b, pa /= pb -> Bottom
--     | a == b, pa == pb -> Meet_ $ GAtom pa a

--   (GIff _ _, GAtom _ _) -> meetFact f2 f1
--   (GAtom pa a, GIff b p)
--     | a == b, pa == True  -> Meet_ $ predToFact p
--     | a == b, pa == False -> Meet_ $ predToFact (PNot p)

--   (GVarIntAbs a i1, GVarIntAbs b i2)
--     | a == b -> Meet_ $ GVarIntAbs a (i1 /\ i2)

--   _ -> Top

-- solveFor :: Name -> GExpr -> GExpr
-- solveFor _ = id

-- resolve :: Tree -> GExpr
-- resolve (GExpr xs) = xs
-- resolve (TAnd t1 t2) = 
--   let x = resolve t1 
--       y = resolve t2
--   in case x `meetExpr` y of
--     GFacts [] -> GChoice x y
--     z         -> z
-- resolve (TImpl t1 t2) = resolve t1 `meetExpr` resolve t2
-- resolve (TAll x _ t) = solveFor x $ resolve t

-- solveFor :: Name -> GExpr -> GExpr
-- solveFor _ = id

-- meetExpr :: GExpr -> GExpr -> GExpr
-- meetExpr (GFacts xs) (GFacts ys) = undefined
-- meetExpr (GChoice xs x2) (GFacts ys) = undefined
-- meetExpr (GFacts xs) (GChoice ys y2) = undefined
-- meetExpr (GChoice xs x2) (GChoice ys y2) = undefined


-- resolve (GOr xs ys) = GOr xs ys
-- resolve (TImpl (resolve -> GExpr xs) (resolve -> GExpr ys)) = GExpr (xs ++ ys)
-- resolve (TImpl (resolve -> GExpr ys) (resolve -> GOr ys zs)) = GExpr
--   (GExpr xs, GOr ys zs) -> GOr (xs ++ ys) (xs ++ zs)  -- TODO: meet
--   (GOr xs ys, GExpr zs) -> GOr (xs ++ zs) (ys ++ zs) -- TODO: meet
--   (t1', t2') -> TImpl t1' t2'


-- resolve (TImpl t1 t2) = case (resolve t1, resolve t2) of
--   (GExpr xs, GExpr ys) -> GExpr (xs ++ ys)  -- TODO: meet
--   (GExpr xs, GOr ys zs) -> GOr (xs ++ ys) (xs ++ zs)  -- TODO: meet
--   (GOr xs ys, GExpr zs) -> GOr (xs ++ zs) (ys ++ zs) -- TODO: meet
--   (t1', t2') -> TImpl t1' t2'
-- resolve (TAll _ _ p) = resolve p  -- TODO: solve for x
-- resolve (TAnd t1 t2) = case (resolve t1, resolve t2) of
--   (GExpr xs, GExpr ys) -> GExpr (xs ++ ys) -- TODO: meet
--   (GExpr xs, GOr ys zs) -> GOr (xs ++ ys) (xs ++ zs)  -- TODO: meet
--   (GOr xs ys, GExpr zs) -> GOr (xs ++ zs) (ys ++ zs) -- TODO: meet
--   (t1', t2') -> TImpl t1' t2'




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