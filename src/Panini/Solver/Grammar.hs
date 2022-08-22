{-# LANGUAGE StrictData #-}

module Panini.Solver.Grammar (solve) where

import Control.Applicative
import Data.Foldable
import Data.Generics.Uniplate.Direct
import Data.Hashable
import Data.HashSet (HashSet)
import Data.HashSet qualified as HS
import Data.List qualified as List
import Data.Maybe
import Data.Set qualified as Set
import Data.Text qualified as Text
import Debug.Trace
import GHC.Generics
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
  in t `seq` PFalse

solve' :: Con -> Tree
solve' = reduce . construct . CAll "s" TString PTrue

-------------------------------------------------------------------------------

data Tree
  = TOr Tree Tree        -- p ∨ q
  | TAnd Tree Tree       -- p ∧ q
  | TImpl Tree Tree      -- p ==> q
  | TIff Tree Tree       -- p <==> q
  | TNot Tree            -- ¬p
  | TAll Name Base Tree  -- ∀x:b. p
  | TSys GSystem
  | TTrue
  | TFalse
  | TUnknown Pred
  deriving stock (Eq, Show, Read)

data GSystem = GSystem (HashSet GRel) 
  deriving stock (Eq, Show, Read)

instance PartialOrd GSystem where
  GSystem xs ⊑ GSystem ys = xs `HS.isSubsetOf` ys

instance MeetSemilattice GSystem where
  --GSystem xs ⊓ GSystem ys = GSystem (xs `HS.union` ys)
  GSystem xs ⊓ GSystem ys = GSystem $ HS.fromList $ partialMeets (xs `HS.union` ys)
-- TODO: become bottom if any rel involves bottom

instance BoundedMeetSemilattice GSystem where
  (⊤) = GSystem mempty

sysSingleton :: GRel -> GSystem
sysSingleton = GSystem . HS.singleton

sysFromList :: [GRel] -> GSystem
sysFromList = GSystem . HS.fromList

sysToList :: GSystem -> [GRel]
sysToList (GSystem xs) = HS.toList xs

data GRel = GRel Rel GExpr GExpr
  deriving stock (Eq, Generic, Show, Read)

instance Hashable GRel

instance Complementable GRel where
  neg (GRel r e1 e2) = GRel (invRel r) e1 e2

instance PartialMeetSemilattice GRel where
  x ⊓? y | x == y = Just x
  
  GRel Eq (GVar x) (GAbs a) ⊓? GRel Eq (GVar y) (GAbs b) | x == y = GRel Eq (GVar x) <$> GAbs <$> (a ⊓? b)

  GRel r a@(GVar _) b ⊓? GRel s c d
    | a == c, Just b' <- mkAbs r b, Just d' <- mkAbs s d = GRel Eq a <$> (b' ⊓? d')
    | a == d, Just b' <- mkAbs r b, Just c' <- mkAbs s c = GRel Eq a <$> (b' ⊓? c')
  
  GRel r a b@(GVar _) ⊓? GRel s c d
    | b == c, Just a' <- mkAbs r a, Just d' <- mkAbs s d = GRel Eq b <$> (a' ⊓? d')
    | b == d, Just a' <- mkAbs r a, Just c' <- mkAbs s c = GRel Eq b <$> (a' ⊓? c')

  _ ⊓? _ = Nothing

data GExpr
  = GVar Name
  | GCon Constant
  | GAbs AbstractValue
  | GStrLen Name
  | GStrAt Name Integer
  deriving stock (Eq, Generic, Show, Read)

instance Hashable GExpr

instance PartialMeetSemilattice GExpr where
  x ⊓? y | x == y = Just x
  GAbs a ⊓? GAbs b = GAbs <$> (a ⊓? b)
  _ ⊓? _ = Nothing

data AbstractValue
  = AInt AInteger
  | ABool ABool
  | AChar AChar
  deriving stock (Eq, Generic, Show, Read)

instance Hashable AbstractValue

instance PartialMeetSemilattice AbstractValue where
  x ⊓? y | x == y = Just x
  AInt  a ⊓? AInt  b = Just $ AInt  (a ⊓ b)
  ABool a ⊓? ABool b = Just $ ABool (a ⊓ b)
  AChar a ⊓? AChar b = Just $ AChar (a ⊓ b)
  _ ⊓? _ = Nothing

mkAbs :: Rel -> GExpr -> Maybe GExpr
mkAbs Eq (GAbs a)         = Just $ GAbs a
mkAbs Ne (GAbs (AInt  a)) = Just $ GAbs $ AInt  $ neg a
mkAbs Ne (GAbs (ABool a)) = Just $ GAbs $ ABool $ neg a
mkAbs Ne (GAbs (AChar a)) = Just $ GAbs $ AChar $ neg a
mkAbs Eq (GCon (I i _))   = Just $ GAbs $ AInt  $ aIntegerEq i
mkAbs Ne (GCon (I i _))   = Just $ GAbs $ AInt  $ aIntegerNe i
mkAbs Lt (GCon (I i _))   = Just $ GAbs $ AInt  $ aIntegerLt i
mkAbs Gt (GCon (I i _))   = Just $ GAbs $ AInt  $ aIntegerGt i
mkAbs Le (GCon (I i _))   = Just $ GAbs $ AInt  $ aIntegerLe i
mkAbs Ge (GCon (I i _))   = Just $ GAbs $ AInt  $ aIntegerGe i
mkAbs Eq (GCon (B b _))   = Just $ GAbs $ ABool $ aBoolEq b
mkAbs Ne (GCon (B b _))   = Just $ GAbs $ ABool $ aBoolEq (not b)
mkAbs Eq (GConChar c)     = Just $ GAbs $ AChar $ aCharEq c
mkAbs Ne (GConChar c)     = Just $ GAbs $ AChar $ aCharNe c
mkAbs _ _                 = Nothing

pattern GConChar :: Char -> GExpr
pattern GConChar c <- GCon (S (Text.unpack -> [c]) _)

instance Uniplate Tree where
  uniplate (TOr   t1 t2) = plate TOr   |* t1 |* t2
  uniplate (TAnd  t1 t2) = plate TAnd  |* t1 |* t2
  uniplate (TImpl t1 t2) = plate TImpl |* t1 |* t2
  uniplate (TIff  t1 t2) = plate TIff  |* t1 |* t2
  uniplate (TNot  t)     = plate TNot  |* t
  uniplate (TAll x b t) = plate (TAll x b) |* t
  uniplate x = plate x

instance GraphViz Tree where
  dot = fromDAG . dag
    where
      dag = \case
        TOr   p q -> CircleNode "∨" [dag p, dag q]
        TAnd  p q -> CircleNode "∧" [dag p, dag q]
        TImpl p q -> CircleNode "⇒" [dag p, dag q]
        TIff  p q -> CircleNode "⇔" [dag p, dag q]
        TNot  p   -> CircleNode "¬" [dag p]
        TAll x b p -> CircleNode (labAll x b) [dag p]
        TSys xs -> BoxNode (labSys xs) []
        TTrue  -> Node [Shape None, Label "⊤"] []
        TFalse -> Node [Shape None, Label "⊥"] []
        TUnknown p -> Node [Shape None, Label (labUnknown p)] []
    
      labAll x b = rend $ "∀" <> pretty x <> ":" <> pretty b
      labSys s = rend $ mconcat $ map ((<> "\\l") . pretty) $ sysToList s
      labUnknown p = rend $ "⟨ " <> pretty p <> " ⟩"    
      rend = renderDoc (RenderOptions False True Nothing)

instance Pretty GRel where
  pretty (GRel r e1 e2) = pretty e1 <> " " <> pretty r <> " " <> pretty e2

instance Pretty GExpr where
  pretty (GVar x) = pretty x
  pretty (GCon c) = pretty c
  pretty (GAbs a) = pretty a
  pretty (GStrLen s) = "|" <> pretty s <> "|"
  pretty (GStrAt s i) = pretty s <> "[" <> pretty i <> "]"

instance Pretty AbstractValue where
  pretty (AInt a) = pretty a
  pretty (ABool a) = pretty a
  pretty (AChar a) = pretty a

-------------------------------------------------------------------------------

construct :: Con -> Tree
construct = goC
  where
    goC (CAnd c1 c2)   = TAnd (goC c1) (goC c2)    
    goC (CAll x b p c) = TAll x b (TImpl (goP p) (goC c))
    goC (CHead p)      = goP p    
    goP (PAnd [p])     = goP p
    goP (PAnd (p:ps))  = TAnd (goP p) (goP (PAnd ps))
    goP (PIff p q)     = TIff (goP p) (goP q)
    goP (PNot p)       = TNot (goP p)
    goP PTrue          = TTrue
    goP PFalse         = TFalse
    goP (PRel r e1 e2) = TSys $ sysSingleton $ GRel r (goE e1) (goE e2)
    goP p              = TUnknown p
    goE (PVar x)       = GVar x
    goE (PCon c)       = GCon c
    goE (PStrLen s)    = GStrLen s
    goE (PStrAt s i)   = GStrAt s i
    goE _              = undefined

pattern PStrLen :: Name -> PExpr
pattern PStrLen s <- PFun "len" [PVar s]

pattern PStrAt :: Name -> Integer -> PExpr
pattern PStrAt s i <- PFun "charat" [PVar s, PCon (I i _)]

-------------------------------------------------------------------------------

reduce :: Tree -> Tree
reduce = rewrite $ \case
  
  TAnd TTrue t2 -> Just t2
  TAnd t1 TTrue -> Just t1
  TAnd TFalse _ -> Just TFalse
  TAnd _ TFalse -> Just TFalse    
  TAnd (TOr t1 t2) t3 -> Just $ TOr (TAnd t1 t3) (TAnd t2 t3)
  TAnd t1 (TOr t2 t3) -> Just $ TOr (TAnd t1 t2) (TAnd t1 t3)
  TAnd (TSys xs) (TSys ys) -> Just $ TSys (xs ⊓ ys)
    
  TOr TFalse t2 -> Just t2
  TOr t1 TFalse -> Just t1
  TOr (TSys xs) (TSys ys)
    | xs ⊑ ys -> Just $ TSys ys
    | ys ⊑ xs -> Just $ TSys xs

  TImpl (TOr t1 t2) t3 -> Just $ TAnd (TImpl t1 t3) (TImpl t2 t3)
  TImpl t1          t2 -> Just $ TOr (TNot t1) (TAnd t1 t2)

  TIff t1 t2 -> Just $ TOr (TAnd t1 t2) (TAnd (TNot t1) (TNot t2))

  TNot (TNot t)       -> Just t
  TNot (TOr t1 t2)    -> Just $ TAnd (TNot t1) (TNot t2)
  TNot (TAnd t1 t2)   -> Just $ TOr (TNot t1) (TNot t2)
  TNot (TImpl t1 t2)  -> Just $ TAnd t1 (TNot t2)
  TNot (TIff t1 t2)   -> Just $ TOr (TAnd (TNot t1) t2) (TAnd t1 (TNot t2))
  TNot TTrue          -> Just TFalse
  TNot TFalse         -> Just TTrue
    
  TNot (TSys xs) -> case sysToList xs of
    [] -> undefined -- TODO
    [x] -> Just $ TSys $ sysSingleton $ neg x
    ys  -> Just $ foldr1 TOr $ map (TSys . sysSingleton . neg) ys
  
  --TAll x _b t -> Just $ eliminate x t

  _ -> Nothing

-------------------------------------------------------------------------------

eliminate :: Name -> Tree -> Tree
eliminate x (TSys s) = TSys (elim x s)
eliminate x (TOr t1 t2) = TOr (eliminate x t1) (eliminate x t2)
eliminate _ t = t

elim :: Name -> GSystem -> GSystem
elim x s = case partialMeets defs of
  [GRel Eq _ e] -> sysFromList $ map (use e) rest
  _ -> GSystem mempty -- TODO: should be bottom, not top
  where
    (defs, rest) = List.partition def $ sysToList s    
    def (GRel _ (GVar y) (GCon _)) = y == x
    def (GRel _ (GCon _) (GVar y)) = y == x
    def _                          = False    

    use e (GRel r a b) = GRel r (useE e a) (useE e b)
    useE e (GVar y) | y == x = e
    --useE e (GStrAt s i) | s == x = GStrAt e i
    --useE e (GStrLen s) | s == x = GStrLen e
    useE _ a = a

  -- get definitions of x in s
  -- combine those definitions into a single one (if not possible, fail)
  -- replace uses with new definition
    

