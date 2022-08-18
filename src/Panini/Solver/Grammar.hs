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
import Debug.Trace
import Data.Set qualified as Set

-------------------------------------------------------------------------------

solve :: Con -> Pred
solve c = 
  let t = traceGraph "trace.svg" $ solve' c
  in t `seq` PFalse

solve' :: Con -> Tree
--solve' = last . iterateUntilStable reduce . treeify
solve' = reduce . treeify . CAll "s" TString PTrue

iterateUntilStable :: (GraphViz a, Eq a) => (a -> a) -> a -> [a]
iterateUntilStable f = go 0 . iterate f
  where
    go _ [] = []
    go _ [x] = [x]
    go !n (x:y:zs) 
      | x == y = [x]
      | otherwise = 
        x : go (n + 1) (y:zs)
        --traceGraph ("trace" ++ show n ++ ".svg") x `seq` x : go (n + 1) (y:zs)

-------------------------------------------------------------------------------

reduce :: Tree -> Tree
reduce = rewrite go
  where
    go (TAnd TTrue t2) = Just t2
    go (TAnd t1 TTrue) = Just t1
    go (TAnd (TTerm xs) (TTerm ys)) = Just $ TTerm (xs ⊓ ys)
    go (TAnd TFalse _) = Just TFalse
    go (TAnd _ TFalse) = Just TFalse
    go (TAnd (TOr t1 t2) t3) = Just $ TOr (TAnd t1 t3) (TAnd t2 t3)
    go (TAnd t1 (TOr t2 t3)) = Just $ TOr (TAnd t1 t2) (TAnd t1 t3)
    
    go (TOr TFalse t2) = Just t2
    go (TOr t1 TFalse) = Just t1
    go (TOr (TTerm xs) (TTerm ys))
      | all (`elem` ys) xs = Just (TTerm ys)
      | all (`elem` xs) ys = Just (TTerm xs)

    go (TTerm xs) | hasBot xs = Just TFalse

    go (TImpl (TOr t1 t2) t3) = Just $ TAnd (TImpl t1 t3) (TImpl t2 t3)
    go (TImpl t1 t2) = Just $ TOr (negT t1) (TAnd t1 t2)

    go (TIff t1 t2) = Just $ TOr (TAnd t1 t2) (TAnd (negT t1) (negT t2))

    go (TAll _ _ t) = Just t

    -- go (TAll _ TUnit t) = Just t
    -- go (TAll x TBool (TTerm xs)) 
    --   | Just xs' <- solveForBool x xs = Just (TTerm xs')
    -- go (TAll x TString (TTerm xs))
    --   | Just xs' <- solveForString x xs = Just (TTerm xs')
    
    -- go (TAll x TInt (TTerm xs))
    --   | Just xs' <- solveForInt x xs = Just (TTerm xs')
    
    -- go (TAll x TInt (TOr (TTerm xs) (TTerm ys)))
    --   | Just xs' <- solveForInt x xs
    --   , Just ys' <- solveForInt x ys 
    --   = Just (TOr (TTerm xs') (TTerm ys'))
    

    go _ = Nothing

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
  TTrue -> TFalse
  TFalse -> TTrue

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
  | TTrue
  | TFalse
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


solveFor :: Name -> APredSet -> Maybe APredSet
solveFor x ps0 = undefined





solveForBool :: Name -> APredSet -> Maybe APredSet
solveForBool x ps = case filter (\p -> isLHS x p || isRHS x p) ps of
  [] -> Just ps
  [p@(AEq v (ABool _))] | v == x -> Just $ ps List.\\ [p]
  _ -> Nothing

solveForString :: Name -> APredSet -> Maybe APredSet
solveForString x ps = case (filter (isLHS x) ps, filter (isRHS x) ps, filter (\p -> not (isLHS x p || isRHS x p)) ps) of
  ([],[],qs) -> Just qs
  ([AEq _ e], [AStrAt s i (AVar _)], qs) -> Just $ AStrAt s i e : qs
  _ -> Nothing

solveForInt :: Name -> APredSet -> Maybe APredSet
solveForInt x ps = case (filter (isLHS x) ps, filter (isRHS x) ps, filter (\p -> not (isLHS x p || isRHS x p)) ps) of
  ([],[],qs) -> Just qs
  ([AEq _ e], [AStrLen s (AVar _)], qs) -> Just $ AStrLen s e : qs
  _ -> Nothing

isLHS :: Name -> APred -> Bool
isLHS v (AEq y _)             = y == v
isLHS v (AStrAt _ (AVar y) _) = y == v
isLHS v (AStrAt y _ _)        = y == v
isLHS v (AStrLen y _)         = y == v
isLHS _ _                     = False

isRHS :: Name -> APred -> Bool
isRHS v (AEq _ (AVar y))      = y == v
isRHS v (AStrAt _ _ (AVar y)) = y == v
isRHS v (AStrLen _ (AVar y))  = y == v
isRHS _ _                     = False


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
      dag TTrue = Node [Shape None, Label "⊤"] []
      dag TFalse = Node [Shape None, Label "⊥"] []
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
    goP  PTrue         = TTrue
    goP  PFalse        = TFalse
    goP p              = TTerm [abstract p]

abstract :: Pred -> APred
abstract = \case
  -- PNot (PCon (B b pv)) -> abstract $ PCon (B (not b) pv)  
  PNot (PRel r x y)    -> abstract $ PRel (invRel r) x y

  -- PVar x        -> AEq x $ ABool $ aBoolEq True
  -- PNot (PVar x) -> AEq x $ ABool $ aBoolEq False
  
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

pattern PConChar :: Char -> PExpr
pattern PConChar c <- PCon (S (Text.unpack -> [c]) _)

pattern PStrLen :: Name -> PExpr
pattern PStrLen x <- PFun "len" [PVar x]

pattern PStrAt :: Name -> PExpr -> PExpr
pattern PStrAt x p <- PFun "charat" [PVar x, p]


aIntegerRel :: Rel -> Integer -> AInteger
aIntegerRel r i = case r of
  Eq -> aIntegerEq i
  Ne -> aIntegerNe i
  Gt -> aIntegerGt i
  Ge -> aIntegerGe i
  Lt -> aIntegerLt i
  Le -> aIntegerLe i
