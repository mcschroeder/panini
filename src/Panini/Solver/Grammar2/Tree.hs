{-# LANGUAGE StrictData #-}

module Panini.Solver.Grammar2.Tree where

import Panini.Algebra.Lattice
import Panini.Syntax
import Prelude
import Panini.Solver.Abstract.ABool
import Panini.Solver.Abstract.AInt
import Panini.Solver.Abstract.AString
import Data.Generics.Uniplate.Direct
import Panini.Pretty.Printer
import Control.Applicative
import Data.HashSet (HashSet)
import Data.HashSet qualified as HashSet
import Data.Hashable
import GHC.Generics (Generic)
import Panini.Pretty.Graphviz
import Data.List (nub)
import Data.Foldable

-------------------------------------------------------------------------------

data Tree
  = TAnd (HashSet Tree)
  | TOr (HashSet Tree)
  | TImp Tree Tree
  | TIff Tree Tree
  | TNeg Tree
  | TAll Name Base Tree
  | TTrue
  | TFalse
  | TPred TPred
  deriving stock (Eq, Show, Read, Generic)

instance Hashable Tree

isAnd :: Tree -> Bool
isAnd (TAnd _) = True
isAnd _        = False

isOr :: Tree -> Bool
isOr (TOr _) = True
isOr _       = False

isPred :: Tree -> Bool
isPred (TPred _) = True
isPred _         = False

instance Uniplate Tree where
  uniplate = \case
    TAnd ts    -> plate TAnd |+ ts
    TOr  ts    -> plate TOr  |+ ts
    TImp t1 t2 -> plate TImp |* t1 |* t2
    TIff t1 t2 -> plate TIff |* t1 |* t2
    TNeg t     -> plate TNeg |* t
    TAll x b t -> plate TAll |- x |- b |* t
    TTrue      -> plate TTrue
    TFalse     -> plate TFalse
    TPred p    -> plate TPred |- p

instance Biplate [Tree] Tree where
  biplate (x:xs) = plate (:) |* x ||* xs
  biplate x      = plate x

instance Biplate (HashSet Tree) Tree where
  biplate = plateProject HashSet.toList HashSet.fromList

instance Biplate Tree TExpr where
  biplate = \case
    TAnd ts    -> plate TAnd |+ ts
    TOr  ts    -> plate TOr  |+ ts
    TImp t1 t2 -> plate TImp |+ t1 |+ t2
    TIff t1 t2 -> plate TIff |+ t1 |+ t2
    TNeg t     -> plate TNeg |+ t
    TAll x b t -> plate TAll |- x |- b |+ t
    TTrue      -> plate TTrue
    TFalse     -> plate TFalse
    TPred p    -> plate TPred |+ p

instance Biplate [Tree] TExpr where
  biplate (x:xs) = plate (:) |+ x ||+ xs
  biplate x      = plate x

instance Biplate (HashSet Tree) TExpr where
  biplate = plateProject HashSet.toList HashSet.fromList

instance Pretty Tree where
  pretty t0 = case t0 of
    TAnd ts -> concatWithOp symAnd $ map (prettyL t0) $ toList ts
    TOr ts -> concatWithOp symOr $ map (prettyL t0) $ toList ts
    TImp t1 t2 -> prettyL t0 t1 <+> symImplies <+> prettyR t0 t2
    TIff t1 t2 -> prettyL t0 t1 <+> symIff <+> prettyR t0 t2
    TNeg t -> symNeg <> parensIf (t `needsParensPrefixedBy` t0) (pretty t)
    TAll x b t -> parens (symAll <> pretty x <> symColon <> pretty b <> symDot <+> pretty t)
    TTrue -> "true"
    TFalse -> "false"
    TPred p -> pretty p

instance HasFixity Tree where
  fixity (TNeg _)   = Prefix
  fixity (TPred _)  = Infix NoAss 4
  fixity (TAnd _)   = Infix NoAss 3
  fixity (TOr _)    = Infix NoAss 3
  fixity (TImp _ _) = Infix NoAss 1
  fixity (TIff _ _) = Infix NoAss 1
  fixity _          = Infix LeftAss 9

instance GraphViz Tree where
  dot = fromDAG . dag
    where
      dag = \case
        TOr   ps -> CircleNode "∨" (map dag $ toList ps)
        TAnd ps 
          | all isPred ps -> CircleNode "∧" [BoxNode (labPreds $ toList ps) []]
          | otherwise -> CircleNode "∧" (map dag $ toList ps)
        TImp p q -> CircleNode "⇒" [dag p, dag q]
        TIff  p q -> CircleNode "⇔" [dag p, dag q]
        TNeg  p   -> CircleNode "¬" [dag p]
        TAll x b p -> CircleNode (labAll x b) [dag p]
        TPred p -> BoxNode (rend $ pretty p) []
        TTrue  -> Node [Shape None, Label "⊤"] []
        TFalse -> Node [Shape None, Label "⊥"] []    
      labAll x b = rend $ "∀" <> pretty x <> ":" <> pretty b
      labPreds ps = rend $ mconcat $ map ((<> "\\l") . pretty) ps
      rend = renderDoc RenderOptions { styling = Nothing, unicode = True, fixedWidth = Nothing }

-------------------------------------------------------------------------------

data TPred 
  = TReg TExpr RE         -- e ∈ RE
  | TRel Rel TExpr TExpr  -- e₁ ⋈ e₂
  deriving stock (Eq, Show, Read, Generic)

instance Hashable TPred

instance Complementable TPred where
  neg (TReg _e _re) = undefined -- TODO
  neg (TRel r e1 e2) = TRel (invRel r) e1 e2

instance Biplate TPred TExpr where
  biplate = \case
    TReg e re -> plate TReg |* e |- re
    TRel r e1 e2 -> plate TRel |- r |* e1 |* e2

varsP :: TPred -> [Name]
varsP = \case
  TReg e _     -> varsE e
  TRel _ e1 e2 -> varsE e1 ++ varsE e2

instance Pretty TPred where
  pretty = \case
    TReg e re -> pretty e <+> "∈" <+> pretty re
    TRel r e1 e2 -> pretty e1 <+> pretty r <+> pretty e2

-------------------------------------------------------------------------------

data TExpr
  = TVal Value
  | TAbs AValue
  | TAdd TExpr TExpr
  | TSub TExpr TExpr
  | TStrLen TExpr
  | TStrAt TExpr TExpr
  | TNot TExpr -- TODO: hack? or permanent solution?
  -- TODO: we need to treat TNot like an operation exclusively on Booleans
  -- TODO: or we have a general Complement operator that could also be regex complement?
  deriving stock (Eq, Show, Read, Generic)

instance Hashable TExpr

instance Uniplate TExpr where
  uniplate = \case
    TVal v -> plate TVal |- v
    TAbs a -> plate TAbs |- a
    TAdd e1 e2 -> plate TAdd |* e1 |* e2
    TSub e1 e2 -> plate TSub |* e1 |* e2
    TStrLen e -> plate TStrLen |* e
    TStrAt e1 e2 -> plate TStrAt |* e1 |* e2
    TNot e -> plate TNot |* e

pattern TVar :: Name -> TExpr
pattern TVar x = TVal (Var x)

pattern TCon :: Constant -> TExpr
pattern TCon c = TVal (Con c)

varsE :: TExpr -> [Name]
varsE = \case
  TVar x       -> [x]
  TAdd e1 e2   -> varsE e1 ++ varsE e2
  TSub e1 e2   -> varsE e1 ++ varsE e2
  TStrLen e    -> varsE e
  TStrAt e1 e2 -> varsE e1 ++ varsE e2
  _            -> []

instance Pretty TExpr where
  pretty p0 = case p0 of
    TVal v -> pretty v
    TAbs a -> pretty a
    TAdd p1 p2 -> prettyL p0 p1 <+> "+" <+> prettyR p0 p2
    TSub p1 p2 -> prettyL p0 p1 <+> "-" <+> prettyR p0 p2
    TStrLen p -> "|" <> pretty p <> "|"
    TStrAt p1 p2 -> pretty p1 <> "[" <> pretty p2 <> "]"
    TNot e -> symNeg <> pretty e

instance HasFixity TExpr where
  fixity (TAdd _ _) = Infix LeftAss 5
  fixity (TSub _ _) = Infix LeftAss 5
  fixity _          = Infix LeftAss 9

-- TODO: allow more expression meets
instance PartialMeetSemilattice TExpr where  
  TAbs a ∧? TAbs b = TAbs <$> a ∧? b
  
   -- TODO: Is this correct?
  TAbs (ABool   a) ∧? e | a == (⊤) = Just e
  TAbs (AInt    a) ∧? e | a == (⊤) = Just e
  TAbs (AString a) ∧? e | a == (⊤) = Just e

  a ∧? b | a == b    = Just a
         | otherwise = Nothing

-------------------------------------------------------------------------------
-- TODO: the stuff below belongs somewhere else

newtype RE = RE String -- TODO
  deriving stock (Eq, Show, Read, Generic)

instance Hashable RE

instance Pretty RE where
  pretty (RE s) = pretty s

data AValue
  = ABool ABool
  | AInt AInt
  | AString AString
  deriving stock (Eq, Show, Read, Generic)

instance Hashable AValue

instance Pretty AValue where
  pretty = \case
    ABool a -> pretty a
    AInt a -> pretty a
    AString a -> pretty a

instance PartialMeetSemilattice AValue where
  ABool   a ∧? ABool   b = Just $ ABool   (a ∧ b)
  AInt    a ∧? AInt    b = Just $ AInt    (a ∧ b)
  AString a ∧? AString b = Just $ AString (a ∧ b)
  _         ∧? _         = Nothing

instance Complementable AValue where
  neg (ABool   a) = ABool   (neg a)
  neg (AInt    a) = AInt    (neg a)
  neg (AString _) = undefined -- TODO: AString (neg a)

-------------------------------------------------------------------------------

-- | Deconstructs a DNF tree into a disjunct list of conjuncts.
-- Warning: Throws an error if the given tree is not in DNF!
-- disjuncts :: Tree -> [[TPred]]
-- disjuncts (TOr xs) = map go $ toList xs
--   where
--     go (TAnd ys) = map go2 $ toList ys
--     go (TPred p) = [p]
--     go x = error $ "unknown " ++ showPretty x
--     go2 (TPred p) = p
--     go2 _ = undefined
-- disjuncts _ = undefined

-- disjuncts t0 = goD t0
--   where
--     goD (TOr d1@(TOr _ _)  d2@(TOr _ _))  = goD d1 ++ goD d2
--     goD (TOr d1@(TOr _ _)  c2@(TAnd _ _)) = goD d1 ++ [goC c2]
--     goD (TOr d1@(TOr _ _)     (TPred p2)) = goD d1 ++ [[p2]]
--     goD (TOr c1@(TAnd _ _) d2@(TOr _ _))  = [goC c1] ++ goD d2
--     goD (TOr c1@(TAnd _ _) c2@(TAnd _ _)) = [goC c1, goC c2]    
--     goD (TOr c1@(TAnd _ _)    (TPred p2)) = [goC c1, [p2]]
--     goD (TOr    (TPred p1) d2@(TOr _ _))  = [[p1]] ++ goD d2
--     goD (TOr    (TPred p1) c2@(TAnd _ _)) = [[p1], goC c2]
--     goD (TOr    (TPred p1)    (TPred p2)) = [[p1], [p2]]
--     goD c                                 = [goC c]
    
--     goC (TAnd t1@(TAnd _ _) t2@(TAnd _ _)) = goC t1 ++ goC t2
--     goC (TAnd t1@(TAnd _ _)    (TPred p2)) = goC t1 ++ [p2]
--     goC (TAnd    (TPred p1) t2@(TAnd _ _)) = [p1] ++ goC t2
--     goC (TAnd    (TPred p1)    (TPred p2)) = [p1,p2]
--     goC (TPred p)                          = [p]
--     goC _                                  = error $ "not in dnf: " ++ showPretty t0

mkDisjuncts :: [[TPred]] -> Tree
mkDisjuncts = TOr . HashSet.fromList . map (TAnd . HashSet.fromList . map TPred) . filter (not . null)


toPreds :: Tree -> [[TPred]]
toPreds t0@(TOr xs) = map go $ toList xs
  where
    go (TAnd ys) = map go2 $ toList ys
    go (TPred p) = [p]
    go x = error $ "expected TAnd or TPred instead of " ++ showPretty x ++ " in " ++ showPretty t0
    go2 (TPred p) = p
    go2 x = error $ "expected TPred instead of " ++ showPretty x ++ " in " ++ showPretty t0
toPreds (TPred p) = [[p]]
toPreds _ = error "expected TOr"



