{-# LANGUAGE OverloadedLists #-}
module Panini.Syntax.Expressions where

import Algebra.Lattice
import Data.Generics.Uniplate.Direct
import Data.Hashable
import Data.List qualified as List
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Generics (Generic)
import Panini.Abstract.AUnit as AUnit
import Panini.Abstract.ABool as ABool
import Panini.Abstract.AInt as AInt
import Panini.Abstract.AChar as AChar
import Panini.Abstract.AString as AString
import Panini.Abstract.AValue as AValue
import Panini.Pretty
import Panini.Provenance
import Panini.Regex.POSIX.ERE (ERE)
import Panini.Syntax.Names
import Panini.Syntax.Primitives
import Panini.Syntax.Substitution
import Prelude

------------------------------------------------------------------------------

-- | Expressions within predicates are built from constants, variables, linear
-- integer arithmetic, functions over strings, and uninterpreted functions.
data Expr  
  = EVar !Name                 -- ^ variable @x@
  | ECon !Value                -- ^ concrete value @c@
  | EAbs !AValue               -- ^ abstract value @α@
  | EReg !ERE                  -- ^ regular expression @RE@
  | ENot !Expr                 -- ^ Boolean negation @¬e@
  | EAdd !Expr !Expr           -- ^ integer addition @e₁ + e₂@
  | ESub !Expr !Expr           -- ^ integer subtraction @e₁ - e₂@
  | EMul !Expr !Expr           -- ^ integer multiplication @e₁ * e₂@
  | EStrLen !Expr              -- ^ string length @|s|@
  | EStrAt !Expr !Expr         -- ^ character at index @s[i]@
  | EStrSub !Expr !Expr !Expr  -- ^ substring @s[i..j]@ (inclusive bounds)
  | EFun !Name ![Expr]         -- ^ uninterpreted function @f(e₁,e₂,…,eₙ)@   
  deriving stock (Eq, Show, Read, Generic)

instance Hashable Expr

pattern EUnit :: PV -> Expr
pattern EUnit pv = ECon (U pv)

pattern EUnitA :: AUnit -> Expr
pattern EUnitA a <- (toAUnit -> Just a) where
  EUnitA a = EAbs (AUnit a)

toAUnit :: Expr -> Maybe AUnit
toAUnit = \case
  EAbs (AUnit a) -> Just a
  EUnit _        -> Just (AUnit.Unit)
  _              -> Nothing

pattern EBool :: Bool -> PV -> Expr
pattern EBool b pv = ECon (B b pv)

pattern EBoolA :: ABool -> Expr
pattern EBoolA a <- (toABool -> Just a) where
  EBoolA a = EAbs (ABool a)

toABool :: Expr -> Maybe ABool
toABool = \case
  EAbs (ABool a) -> Just a
  EBool b _      -> Just (ABool.eq b)
  _              -> Nothing

pattern EInt :: Integer -> PV -> Expr
pattern EInt i pv = ECon (I i pv)

pattern EIntA :: AInt -> Expr
pattern EIntA a <- (toAInt -> Just a) where
  EIntA a = EAbs (AInt a)

toAInt :: Expr -> Maybe AInt
toAInt = \case
  EAbs (AInt a) -> Just a
  EInt n _      -> Just (AInt.eq n)
  _             -> Nothing

pattern EStr :: Text -> PV -> Expr
pattern EStr s pv = ECon (S s pv)

pattern EStrA :: AString -> Expr
pattern EStrA a <- (toAString -> Just a) where
  EStrA a = EAbs (AString a)

toAString :: Expr -> Maybe AString
toAString = \case
  EAbs (AString a) -> Just a
  EStr s _         -> Just (AString.eq $ Text.unpack s)
  _                -> Nothing

pattern EChar :: Char -> PV -> Expr
pattern EChar c pv <- ECon (S (Text.unpack -> [c]) pv) where
  EChar c pv = ECon (S (Text.pack [c]) pv)

pattern ECharA :: AChar -> Expr
pattern ECharA a <- (toAChar -> Just a)  where
  ECharA a = EAbs (AString (lit a))

toAChar :: Expr -> Maybe AChar
toAChar = \case
  EAbs (AString s) -> AString.toChar s
  EChar c _        -> Just $ AChar.eq c
  _                -> Nothing

pattern (:+:) :: Expr -> Expr -> Expr
pattern e1 :+: e2 = EAdd e1 e2

pattern (:-:) :: Expr -> Expr -> Expr
pattern e1 :-: e2 = ESub e1 e2

pattern StrAt_index :: Expr -> Expr -> Expr
pattern StrAt_index s c = EFun "_StrAt_index" [s,c]

pattern StrSub_index2 :: Expr -> Expr -> Expr -> Expr
pattern StrSub_index2 s t i = EFun "_StrSub_index_end" [s,t,i]

pattern StrComp :: Expr -> Expr
pattern StrComp e = EFun "_StrComplement" [e]

pattern IntComp :: Expr -> Expr
pattern IntComp e = EFun "_IntComplement" [e]

------------------------------------------------------------------------------

topExpr :: Base -> Expr
topExpr TUnit   = EUnitA top
topExpr TBool   = EBoolA top
topExpr TInt    = EIntA top
topExpr TString = EStrA top

botExpr :: Base -> Expr
botExpr TUnit   = EUnitA bot
botExpr TBool   = EBoolA bot
botExpr TInt    = EIntA bot
botExpr TString = EStrA bot

typeOfExpr :: Expr -> Maybe Base
typeOfExpr = \case
  EVar _        -> Nothing
  ECon v        -> Just $ typeOfValue v
  EAbs a        -> Just $ typeOfAValue a
  EReg _        -> Just TString
  ENot _        -> Just TBool
  EAdd _ _      -> Just TInt
  ESub _ _      -> Just TInt
  EMul _ _      -> Just TInt
  EStrLen _     -> Just TInt
  EStrAt _ _    -> Just TString
  EStrSub _ _ _ -> Just TString
  EFun _ _      -> Nothing

eqTypeAE :: AValue -> Expr -> Bool
eqTypeAE a e =  maybe True (typeOfAValue a ==) (typeOfExpr e)
-- note: we assume that variables are always of the right type
-- TODO: have vars track their types
-- TODO: add a predicate typechecking pass

------------------------------------------------------------------------------

instance Uniplate Expr where
  uniplate = \case
    EVar x           -> plate EVar |- x
    ECon c           -> plate ECon |- c
    EAbs a           -> plate EAbs |- a
    EReg r           -> plate EReg |- r
    ENot e           -> plate ENot |* e
    EAdd e1 e2       -> plate EAdd |* e1 |* e2
    ESub e1 e2       -> plate ESub |* e1 |* e2
    EMul e1 e2       -> plate EMul |* e1 |* e2
    EStrLen e1       -> plate EStrLen |* e1
    EStrAt e1 e2     -> plate EStrAt  |* e1 |* e2
    EStrSub e1 e2 e3 -> plate EStrSub |* e1 |* e2 |* e3
    EFun f es        -> plate EFun |- f ||* es

instance Biplate Expr Value where
  biplate = \case
    EVar x           -> plate EVar |- x
    ECon c           -> plate ECon |* c
    EAbs a           -> plate EAbs |- a
    EReg r           -> plate EReg |- r
    ENot e           -> plate ENot |+ e
    EAdd e1 e2       -> plate EAdd |+ e1 |+ e2
    ESub e1 e2       -> plate ESub |+ e1 |+ e2
    EMul e1 e2       -> plate EMul |+ e1 |+ e2
    EStrLen e1       -> plate EStrLen |+ e1
    EStrAt e1 e2     -> plate EStrAt  |+ e1 |+ e2
    EStrSub e1 e2 e3 -> plate EStrSub |+ e1 |+ e2 |+ e3
    EFun f es        -> plate EFun |- f ||+ es

instance Pretty Expr where
  pretty p0 = case p0 of
    EVar x -> pretty x
    ECon c -> pretty c
    EFun f ps -> pretty f <> parens (mconcat $ List.intersperse ", " $ map pretty ps)
    EMul p1 p2 -> prettyL p0 p1 <+> "*" <+> prettyR p0 p2
    EAdd p1 p2 -> prettyL p0 p1 <+> "+" <+> prettyR p0 p2
    ESub p1 p2 -> prettyL p0 p1 <+> "-" <+> prettyR p0 p2
    EStrLen p -> "|" <> pretty p <> "|"
    EStrAt p1 p2 -> pretty p1 <> "[" <> pretty p2 <> "]"
    EStrSub p1 p2 p3 -> 
      pretty p1 <> "[" <> pretty p2 <> ".." <> pretty p3 <> "]"
    ENot e -> symNeg <> parensIf (complex e) (pretty e)
    EAbs a -> pretty a
    EReg r -> pretty r
   where
    -- TODO: make use of fixity for this
    complex (EMul _ _) = True
    complex (EAdd _ _) = True
    complex (ESub _ _) = True
    complex _ = False

instance HasFixity Expr where
  fixity (EMul _ _) = Infix LeftAss 6
  fixity (EAdd _ _) = Infix LeftAss 5
  fixity (ESub _ _) = Infix LeftAss 5
  fixity _          = Infix LeftAss 9

instance Subable Expr Expr where
  subst x y = \case
    EVar n | y == n -> x
    e               -> descend (subst x y) e

  freeVars = \case
    EVar x           -> [x]
    ECon _           -> []
    EAbs _           -> []
    EReg _           -> []
    ENot e           -> freeVars e
    EAdd e1 e2       -> freeVars e1 <> freeVars e2
    ESub e1 e2       -> freeVars e1 <> freeVars e2
    EMul e1 e2       -> freeVars e1 <> freeVars e2
    EStrLen e        -> freeVars e
    EStrAt e1 e2     -> freeVars e1 <> freeVars e2
    EStrSub e1 e2 e3 -> freeVars e1 <> freeVars e2 <> freeVars e3
    EFun _ es        -> mconcat (map freeVars es)

instance PartialMeetSemilattice Expr where  
  EAbs a ∧? EAbs b = EAbs <$> a ∧? b  
  EAbs a ∧? e      | containsTop a, eqTypeAE a e = Just e
  EAbs a ∧? e      | containsBot a, eqTypeAE a e = Just $ EAbs $ fillBot a
  e      ∧? EAbs a | containsTop a, eqTypeAE a e = Just e
  e      ∧? EAbs a | containsBot a, eqTypeAE a e = Just $ EAbs $ fillBot a
  
  ECon a ∧? ECon b = EAbs <$> fromValue a ∧? fromValue b
  ECon a ∧? EAbs b = EAbs <$> fromValue a ∧? b
  EAbs a ∧? ECon b = EAbs <$> fromValue b ∧? a

  (x :+: a) ∧? (y :+: b) | x == y = (x :+:) <$> a ∧? b
  (x :+: a) ∧? (b :+: y) | x == y = (x :+:) <$> a ∧? b
  (a :+: x) ∧? (y :+: b) | x == y = (x :+:) <$> a ∧? b
  (a :+: x) ∧? (b :+: y) | x == y = (x :+:) <$> a ∧? b
  (x :+: a) ∧? (y :-: b) | x == y = (x :+:) <$> a ∧? (normExpr $ EIntA (AInt.eq 0) :-: b)
  (x :+: _) ∧? (_ :-: y) | x == y = Nothing
  (a :+: x) ∧? (y :-: b) | x == y = (x :+:) <$> a ∧? (normExpr $ EIntA (AInt.eq 0) :-: b)
  (_ :+: x) ∧? (_ :-: y) | x == y = Nothing
  (x :+: a) ∧? (y      ) | x == y = (x :+:) <$> a ∧? EIntA (AInt.eq 0)
  (a :+: x) ∧? (y      ) | x == y = (x :+:) <$> a ∧? EIntA (AInt.eq 0)
  (x :-: a) ∧? (y :+: b) | x == y = (x :+:) <$> (normExpr $ EIntA (AInt.eq 0) :-: a) ∧? b
  (x :-: a) ∧? (b :+: y) | x == y = (x :+:) <$> (normExpr $ EIntA (AInt.eq 0) :-: a) ∧? b
  (_ :-: x) ∧? (y :+: _) | x == y = Nothing
  (_ :-: x) ∧? (_ :+: y) | x == y = Nothing
  (x :-: a) ∧? (y :-: b) | x == y = (x :-:) <$> a ∧? b
  (x :-: _) ∧? (_ :-: y) | x == y = Nothing  
  (_ :-: x) ∧? (y :-: _) | x == y = Nothing
  (_ :-: x) ∧? (_ :-: y) | x == y = Nothing
  (x :-: a) ∧? (y      ) | x == y = (x :-:) <$> a ∧? EIntA (AInt.eq 0)
  (_ :-: x) ∧? (y      ) | x == y = Nothing
  (x      ) ∧? (y :+: b) | x == y = (x :+:) <$> EIntA (AInt.eq 0) ∧? b
  (x      ) ∧? (b :+: y) | x == y = (x :+:) <$> EIntA (AInt.eq 0) ∧? b
  (x      ) ∧? (y :-: b) | x == y = (x :-:) <$> EIntA (AInt.eq 0) ∧? b  
  (x      ) ∧? (_ :-: y) | x == y = Nothing

  a ∧? b | a == b    = Just a
         | otherwise = Nothing

------------------------------------------------------------------------------

-- | Normalize an abstract expression by (partial) evaluation. Note that this
-- operation will never introduce any abstract values; if the input expression
-- did not contain anything abstract, then neither will the normalized output.
normExpr :: Expr -> Expr
normExpr = rewrite $ \case

  ENot (EBool  a pv) -> Just $ EBool  (not a) pv
  ENot (EBoolA a)    -> Just $ EBoolA (neg a)

  EInt  a _ :+: EInt  b _ -> Just $ EInt (a + b) NoPV
  EIntA a   :+: EIntA b   -> Just $ EIntA $ AInt.add a b
  EInt  0 _ :+: e         -> Just e
  e         :+: EInt  0 _ -> Just e
  EIntA a   :+: e         | [0] <- AInt.values a -> Just e
  e         :+: EIntA a   | [0] <- AInt.values a -> Just e
  EIntA a   :+: _         | isBot a -> Just $ EIntA bot  
  _         :+: EIntA a   | isBot a -> Just $ EIntA bot

  EInt  a _ :-: EInt  b _ -> Just $ EInt (a - b) NoPV
  EIntA a   :-: EIntA b   -> Just $ EIntA $ AInt.sub a b
  e         :-: EInt  0 _ -> Just e
  e         :-: EIntA a   | [0] <- AInt.values a -> Just e
  EIntA a   :-: _         | isBot a -> Just $ EIntA bot
  _         :-: EIntA a   | isBot a -> Just $ EIntA bot

  -- re-associate addition/subtraction to get more rewriting opportunities
  (e1 :+: e2) :+: e3 -> Just $ e1 :+: (e2 :+: e3)
  (e1 :+: e2) :-: e3 -> Just $ e1 :+: (e2 :-: e3)  
  (e1 :-: e2) :+: e3 -> Just $ e1 :-: (e2 :-: e3)
  (e1 :-: e2) :-: e3 -> Just $ e1 :-: (e2 :+: e3)

  EStrLen (EStr s _) -> Just $ EInt (fromIntegral $ Text.length s) NoPV

  EStrLen (EStrA a) | isTop a -> Just $ EIntA $ AInt.ge 0
  
  EStrAt (EStr s _) (EInt (fromIntegral -> i) _)
    | i < Text.length s 
    -> Just $ EStr (Text.pack [Text.index s i]) NoPV    
  
  EStrSub (EStr s _) (EInt (fromIntegral -> i) _) (EInt (fromIntegral -> j) _)
    | let n = Text.length s, i < n, j < n, i <= j
    -> Just $ EStr (Text.take (j - i + 1) $ Text.drop i s) NoPV

  StrComp (EStrA s) -> Just $ EStrA $ neg s

  _ -> Nothing
