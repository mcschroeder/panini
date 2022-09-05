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
import Data.Text qualified as Text
import Debug.Trace
import GHC.Generics
import Panini.Pretty.Graphviz
import Panini.Pretty.Printer
import Panini.Solver.Abstract.ABool
import Panini.Solver.Abstract.AChar
import Panini.Solver.Abstract.AChar qualified as AC
import Panini.Solver.Abstract.AInteger
import Panini.Solver.Abstract.AInteger qualified as AI
import Panini.Solver.Abstract.Lattice
import Panini.Syntax
import Prelude

-------------------------------------------------------------------------------

solve :: Con -> Pred
solve c = 
  let t = traceGraph "trace.svg" $ solve' c
  in trace (showPretty t) t `seq` destruct t

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
    | a == d, Just b' <- mkAbs r b, Just c' <- mkAbs (convRel s) c = GRel Eq a <$> (b' ⊓? c')

  GRel r a b@(GVar _) ⊓? GRel s c d
    | b == c, Just a' <- mkAbs (convRel r) a, Just d' <- mkAbs s d = GRel Eq b <$> (a' ⊓? d')
    | b == d, Just a' <- mkAbs (convRel r) a, Just c' <- mkAbs (convRel s) c = GRel Eq b <$> (a' ⊓? c')

  GRel r a@(GStrLen _) b ⊓? GRel s c d
    | a == c, Just b' <- mkAbs r b, Just d' <- mkAbs s d = GRel Eq a <$> (b' ⊓? d')
    | a == d, Just b' <- mkAbs r b, Just c' <- mkAbs (convRel s) c = GRel Eq a <$> (b' ⊓? c')

  GRel r a b@(GStrLen _) ⊓? GRel s c d
    | b == c, Just a' <- mkAbs (convRel r) a, Just d' <- mkAbs s d = GRel Eq b <$> (a' ⊓? d')
    | b == d, Just a' <- mkAbs (convRel r) a, Just c' <- mkAbs (convRel s) c = GRel Eq b <$> (a' ⊓? c')

  _ ⊓? _ = Nothing

hasBot :: GRel -> Bool
hasBot (GRel _ e1 e2) = go e1 || go e2
  where
    go (GAbs (AInt  a)) = a == (⊥)
    go (GAbs (ABool a)) = a == (⊥)
    go (GAbs (AChar a)) = a == (⊥)
    go _                = False

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

instance Complementable AbstractValue where
  neg (AInt  a) = AInt  (neg a)
  neg (ABool a) = ABool (neg a)
  neg (AChar a) = AChar (neg a)

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
      rend = renderDoc RenderOptions { styling = Nothing, unicode = True, fixedWidth = Nothing }

instance Pretty Tree where
  pretty = \case
    TOr t1 t2 -> pretty t1 <> "\\/" <> pretty t2
    TAnd t1 t2 -> pretty t1 <> "/\\" <> pretty t2
    TImpl t1 t2 -> pretty t1 <> "==>" <> pretty t2
    TIff t1 t2 -> pretty t1 <> "<==>" <> pretty t2
    TNot t1 -> "~" <> pretty t1
    TAll x b t -> "forall " <> pretty x <> ":" <> pretty b <> ". " <> pretty t
    TSys s -> "{" <> (mconcat $ List.intersperse ", " $ map pretty $ sysToList s) <> "}"
    TTrue -> "⊤"
    TFalse -> "⊥"
    TUnknown p -> pretty p

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

instance Pretty GSystem where
  pretty (GSystem xs) = 
    "{" <> (mconcat $ List.intersperse ", " $ map pretty $ HS.toList xs) <> "}"

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
    goE (PStrLen (PVar s)) = GStrLen s  -- TODO
    goE (PStrAt (PVar s) (PCon (I i _))) = GStrAt s i  -- TODO
    goE _              = undefined

destruct :: Tree -> Pred
destruct = goT
  where
    goT (TOr t1 t2) = goT t1 `pOr` goT t2
    goT (TSys s)    = PAnd $ map goR $ sysToList s
    goT _           = undefined

    goR (GRel Eq e1 (GAbs (AInt  a))) = AI.toPred (goE e1) a
    goR (GRel Eq e1 (GAbs (AChar a))) = AC.toPred (goE e1) a

    goR (GRel r e1 e2) = PRel r (goE e1) (goE e2)

    goE (GVar x)     = PVar x
    goE (GCon c)     = PCon c
    goE (GStrLen s)  = PStrLen (PVar s)
    goE (GStrAt s i) = PStrAt (PVar s) (PCon (I i NoPV))
    goE _            = undefined

-------------------------------------------------------------------------------

-- TODO: maybe we don't really need TNot, if we only ever negate TSys?

reduce :: Tree -> Tree
reduce = rewrite $ \case

  TOr TTrue     _                   -> Just TTrue
  TOr _         TTrue               -> Just TTrue 
  TOr TFalse    t2                  -> Just t2
  TOr t1        TFalse              -> Just t1
  TOr (TSys xs) (TSys ys) | xs ⊑ ys -> Just $ TSys ys
  TOr (TSys xs) (TSys ys) | ys ⊑ xs -> Just $ TSys xs
  
  TAnd TTrue       t2          -> Just t2
  TAnd t1          TTrue       -> Just t1
  TAnd TFalse      _           -> Just TFalse
  TAnd _           TFalse      -> Just TFalse
  TAnd t1          (TOr t2 t3) -> Just $ TOr (TAnd t1 t2) (TAnd t1 t3)
  TAnd (TOr t1 t2) t3          -> Just $ TOr (TAnd t1 t3) (TAnd t2 t3)
  TAnd (TSys xs)   (TSys ys)   -> Just $ TSys (xs ⊓ ys)

  TImpl TTrue        t           -> Just t
  TImpl t            TTrue       -> Just $ TOr t (TNot t)
  TImpl TFalse       _           -> Just TTrue
  TImpl t            TFalse      -> Just $ TNot t
  TImpl t1           (TOr t2 t3) -> Just $ TOr (TImpl t1 t2) (TImpl t1 t3)
  TImpl (TOr t1 t2)  t3          -> Just $ TAnd (TImpl t1 t3) (TImpl t2 t3)
  TImpl (TAnd t1 t2) t3          -> Just $ TOr (TImpl t1 t3) (TImpl t2 t3)
  TImpl t1           t2          -> Just $ TOr (TNot t1) (TAnd t1 t2)

  TIff t1 t2 -> Just $ TAnd (TImpl t1 t2) (TImpl t2 t1)

  TNot TTrue     -> Just TFalse
  TNot TFalse    -> Just TTrue
  TNot (TNot t)  -> Just t  
  TNot (TSys xs) -> case sysToList xs of
    [x] -> Just $ TSys $ sysSingleton $ neg x
    _   -> Nothing  -- TODO: is it really possible to never need this?

  TSys xs | any hasBot (sysToList xs) -> Just TFalse

  TAll x b t -> Just $ eliminate x b t
  
  _ -> Nothing

-------------------------------------------------------------------------------

eliminate :: Name -> Base -> Tree -> Tree
eliminate _ TUnit t = t
eliminate x b (TSys s) = case partialMeets (topdef:defs) of
  [] -> TSys $ sysFromList rest
  [GRel r _ e] | Just e' <- mkAbs r e -> 
    let zs = map (use e') rest 
    in if null zs then TTrue else TSys $ sysFromList zs
  pfff -> error $ unlines $ map showPretty pfff --TFalse
  where
    topdef = case b of
      TInt -> GRel Eq (GVar x) (GAbs (AInt (⊤)))
      TBool -> GRel Eq (GVar x) (GAbs (ABool (⊤)))
      TString -> GRel Eq (GVar x) (GAbs (AChar (⊤))) -- TODO

    (defs, rest) = List.partition def $ sysToList s    
    def (GRel _ (GVar y) (GCon _)) = y == x
    def (GRel _ (GVar y) (GAbs _)) = y == x
    def (GRel _ (GCon _) (GVar y)) = y == x
    def (GRel _ (GAbs _) (GVar y)) = y == x
    def _                          = False    

    use e (GRel r e1 e2) = norm $ GRel r (useE e e1) (useE e e2)
    useE e (GVar y) | y == x = e
    --useE e (GStrAt s i) | s == x = GStrAt e i
    --useE e (GStrLen s) | s == x = GStrLen e
    useE _ a = a

eliminate x b t = descend (eliminate x b) t

norm :: GRel -> GRel
norm = \case
  GRel r a@(GCon _) b@(GVar _)     -> norm $ GRel (convRel r) b a
  GRel r a@(GAbs _) b@(GVar _)     -> norm $ GRel (convRel r) b a
  GRel r a@(GVar _) b@(GStrAt _ _) -> norm $ GRel (convRel r) b a
  GRel r a@(GCon _) b@(GStrAt _ _) -> norm $ GRel (convRel r) b a
  GRel r a@(GAbs _) b@(GStrAt _ _) -> norm $ GRel (convRel r) b a
  GRel r a@(GCon _) b@(GStrLen _)  -> norm $ GRel (convRel r) b a
  GRel r a@(GAbs _) b@(GStrLen _)  -> norm $ GRel (convRel r) b a

  GRel Ne a (GAbs b) -> GRel Eq a (GAbs (neg b))

  x -> x
