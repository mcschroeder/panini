{-# LANGUAGE StrictData #-}

module Panini.Solver.Grammar (solve) where

import Data.Generics.Uniplate.Direct
import Data.List qualified as List
import Data.Text qualified as Text
import Panini.Pretty.Graphviz
import Panini.Pretty.Printer
import Panini.Solver.Abstract.ABool
import Panini.Solver.Abstract.AChar
import Panini.Solver.Abstract.AInteger
import Panini.Solver.Abstract.Lattice
import Panini.Syntax
import Prelude

-------------------------------------------------------------------------------

solve :: Con -> Pred
solve c = 
  let t = traceGraph "trace.svg" $ solve' c
  in t `seq` PFalse NoPV

solve' :: Con -> Tree
solve' = last . iterateUntilStable reduce . treeify

iterateUntilStable :: (GraphViz a, Eq a) => (a -> a) -> a -> [a]
iterateUntilStable f = go 0 . iterate f
  where
    go _ [] = []
    go _ [x] = [x]
    go !n (x:y:zs) 
      | x == y = [x]
      | otherwise = 
        traceGraph ("trace" ++ show n ++ ".svg") x `seq` x : go (n + 1) (y:zs)

-------------------------------------------------------------------------------

reduce :: Tree -> Tree
reduce = transform red
  where
    red (TAnd (TTerm xs)  (TTerm ys))  = TTerm (xs ⊓ ys)
    red (TAnd (TOr t1 t2) t3)          = TOr (TAnd t1 t3) (TAnd t2 t3)
    red (TAnd t3          (TOr t1 t2)) = TOr (TAnd t1 t3) (TAnd t2 t3)
    
    red (TOr (TTerm xs) t2) | hasBot xs = t2
    red (TOr t1 (TTerm ys)) | hasBot ys = t1
    
    red (TImpl t1 t2) = TOr (negT t1) (TAnd t1 t2)

    red (TIff t1 t2) = TAnd (TImpl t1 t2) (TImpl t2 t1)

    red (TAll _ _ t) = t

    red x = x


hasBot :: APredSet -> Bool
hasBot = any go
  where
    go (AEq _ e) = go2 e
    go (AStrAt _ _ e2) = go2 e2
    go (AStrLen _ e) = go2 e
    go (AUnknown _) = False

    go2 (AInteger a) = a == (⊥)
    go2 (ABool a) = a == (⊥)
    go2 (AChar a) = a == (⊥)
    go2 (AVar _) = False

negT :: Tree -> Tree
negT = \case
  TOr t1 t2 -> TAnd (negT t1) (negT t2)
  TAnd t1 t2 -> TOr (negT t1) (negT t2)
  TImpl t1 t2 -> TAnd t1 (negT t2)
  TAll x b t -> TAll x b (negT t) -- TODO: ???
  TIff t1 t2 -> TOr (TAnd (negT t1) t2) (TAnd t1 (negT t2))
  TTerm xs -> TTerm (negA xs)

negA :: APredSet -> APredSet
negA = map go
  where
    go (AEq x e) = AEq x (go2 e)
    go (AStrAt x e1 e2) = AStrAt x e1 (go2 e2)
    go (AStrLen x e) = AStrLen x (go2 e)
    go (AUnknown p) = AUnknown (PNot p)

    go2 (AInteger a) = AInteger (neg a)
    go2 (ABool a) = ABool (neg a)
    go2 (AChar a) = AChar (neg a)
    go2 (AVar x) = AVar x -- TODO: ?????

data Tree
  = TOr Tree Tree        -- p ∨ q
  | TAnd Tree Tree       -- p ∧ q
  | TImpl Tree Tree      -- p ==> q
  | TAll Name Base Tree  -- ∀x:b. p
  | TIff Tree Tree       -- p <==> q
  | TTerm APredSet
  deriving stock (Eq, Show, Read)

instance Uniplate Tree where
  uniplate (TOr   t1 t2) = plate TOr   |* t1 |* t2
  uniplate (TAnd  t1 t2) = plate TAnd  |* t1 |* t2
  uniplate (TImpl t1 t2) = plate TImpl |* t1 |* t2
  uniplate (TIff  t1 t2) = plate TIff  |* t1 |* t2
  uniplate (TAll x b t) = plate (TAll x b) |* t
  uniplate x = plate x

type APredSet = [APred]

-- TODO: abstract variables at forall, use symbolic rep beforehand?

instance MeetSemilattice APredSet where
  xs ⊓ ys = partialMeets (xs ++ ys)

data APred
  = AEq Name AExpr           -- ^ x = α
  | AStrAt Name AExpr AExpr  -- ^ x[α] = β
  | AStrLen Name AExpr       -- ^ |x| = α
  | AUnknown Pred
  deriving stock (Eq, Show, Read)

instance PartialMeetSemilattice APred where
  AEq x a      ⊓? AEq y b      | x == y         = AEq x      <$> a ⊓? b
  AStrAt x i a ⊓? AStrAt y j b | x == y, i == j = AStrAt x a <$> a ⊓? b
  AStrLen x a  ⊓? AStrLen y b  | x == y         = AStrLen x  <$> a ⊓? b
  _            ⊓? _                             = Nothing

data AExpr
  = AInteger AInteger
  | ABool ABool
  | AChar AChar
  | AVar Name
  deriving stock (Eq, Show, Read)

instance PartialMeetSemilattice AExpr where
  AInteger a ⊓? AInteger b = Just $ AInteger $ a ⊓ b
  ABool a    ⊓? ABool b    = Just $ ABool    $ a ⊓ b
  AChar a    ⊓? AChar b    = Just $ AChar    $ a ⊓ b
  _          ⊓? _          = Nothing

-------------------------------------------------------------------------------

instance GraphViz Tree where
  dot = fromDAG . dag
    where
      dag (TOr   p q) = CircleNode "∨" [dag p, dag q]
      dag (TAnd  p q) = CircleNode "∧" [dag p, dag q]
      dag (TImpl  p q) = CircleNode "⇒" [dag p, dag q]
      dag (TAll x b p) = CircleNode (allLabel x b) [dag p]
      dag (TIff p q) = CircleNode "⇔" [dag p, dag q]
      dag (TTerm fs)  = BoxNode (termLabel fs) []
      termLabel [x] = rend $ pretty x
      termLabel xs = mconcat $ map ((<> "\\l")) $ List.sort $ map (rend . pretty) xs
      allLabel x b = rend $ "∀" <> pretty x <> ":" <> pretty b
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

-- TODO: thought: the tree is a just a reification of the analysis process,
-- i.e., the (Heyting?) algebra, where each op (/\, \/, ==>, ...) is explicit

treeify :: Con -> Tree
treeify = goC
  where
    goC (CAnd c1 c2)   = TAnd (goC c1) (goC c2)    
    goC (CAll x b p c) = TAll x b (TImpl (goP p) (goC c))
    goC (CHead p)      = goP p    
    goP (PAnd [p])     = goP p
    goP (PAnd (p:ps))  = TAnd (goP p) (goP (PAnd ps))
    goP (PIff p q)     = TIff (goP p) (goP q)
    goP p              = TTerm [abstract p]

abstract :: Pred -> APred
abstract = \case
  PNot (PCon (B b pv)) -> abstract $ PCon (B (not b) pv)  
  PNot (PRel r x y)    -> abstract $ PRel (invRel r) x y

  PVar x        -> AEq x $ ABool $ aBoolEq True
  PNot (PVar x) -> AEq x $ ABool $ aBoolEq False
  
  PRel r y@(PCon _) x@(PVar _) -> abstract $ PRel (convRel r) x y
  PRel r  (PVar x) (PCon (I i _)) -> AEq x $ AInteger $ aIntegerRel r i
  PRel Eq (PVar x) (PCon (B b _)) -> AEq x $ ABool $ aBoolEq b
  PRel Ne (PVar x) (PCon (B b _)) -> AEq x $ ABool $ aBoolEq (not b)
  PRel Eq (PVar x) (PConChar c)   -> AEq x $ AChar $ aCharEq c
  PRel Ne (PVar x) (PConChar c)   -> AEq x $ AChar $ aCharNe c
  
  PRel Eq (PVar x) (PVar y) -> AEq x (AVar y)

  PRel r y x@(PStrLen _) -> abstract $ PRel (convRel r) x y
  PRel r  (PStrLen s) (PCon (I i _)) -> AStrLen s $ AInteger $ aIntegerRel r i
  PRel Eq (PStrLen s) (PVar x)       -> AStrLen s $ AVar x

  PRel r y x@(PStrAt _ _) -> abstract $ PRel (convRel r) x y  
  PRel Eq (PStrAt s (PCon (I i _))) (PConChar c) -> AStrAt s (AInteger $ aIntegerEq i) (AChar $ aCharEq c)
  PRel Ne (PStrAt s (PCon (I i _))) (PConChar c) -> AStrAt s (AInteger $ aIntegerEq i) (AChar $ aCharNe c)
  PRel Eq (PStrAt s (PCon (I i _))) (PVar y)     -> AStrAt s (AInteger $ aIntegerEq i) (AVar y)  
  PRel Eq (PStrAt s (PVar x))       (PConChar c) -> AStrAt s (AVar x) (AChar $ aCharEq c)
  PRel Ne (PStrAt s (PVar x))       (PConChar c) -> AStrAt s (AVar x) (AChar $ aCharNe c)
  PRel Eq (PStrAt s (PVar x))       (PVar y)     -> AStrAt s (AVar x) (AVar y)

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
  Eq -> Ne
  Ne -> Eq
  Ge -> Lt
  Le -> Gt
  Gt -> Le
  Lt -> Ge

-- | Converse of a relation, e.g., ≥ to ≤.
convRel :: Rel -> Rel
convRel = \case
  Eq -> Eq
  Ne -> Ne
  Ge -> Le
  Le -> Ge
  Gt -> Lt
  Lt -> Gt

aIntegerRel :: Rel -> Integer -> AInteger
aIntegerRel r i = case r of
  Eq -> aIntegerEq i
  Ne -> aIntegerNe i
  Gt -> aIntegerGt i
  Ge -> aIntegerGe i
  Lt -> aIntegerLt i
  Le -> aIntegerLe i

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