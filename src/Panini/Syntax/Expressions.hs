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
import Panini.Abstract.AInt qualified as AInt
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
  deriving stock 
    ( Eq
    , Ord  -- ^ structural ordering
    , Show, Read, Generic
    )

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

instance PartialOrder Expr where
  EAbs a ⊑ EAbs b = a ⊑ b
  ECon a ⊑ EAbs b = fromValue a ⊑ b
  _      ⊑ EAbs b = containsTop b
  EAbs a ⊑ ECon b = a ⊑ fromValue b
  EAbs a ⊑ _      = containsBot a
  a      ⊑ b      = a <= b
    
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

------------------------------------------------------------------------------

-- | Relation between expressions.
data Rel = Rel !Rop !Expr !Expr    -- ^ binary relation @e₁ ⋈ e₂@  
  deriving stock 
    ( Eq
    , Ord  -- ^ structural ordering
    , Show, Read, Generic
    )

-- | A relation between two expressions.
data Rop 
  = Eq  -- ^ equal @=@
  | Ne  -- ^ unequal @≠@
  | Ge  -- ^ greater than or equal @≥@
  | Le  -- ^ less than or equal @≤@
  | Gt  -- ^ greater than @>@
  | Lt  -- ^ less than @<@
  | In  -- ^ included in @∈@
  | Ni  -- ^ includes @∋@
  | NotIn  -- ^ not included in @∉@
  | NotNi  -- ^ does not include @∌@
  deriving stock (Eq, Ord, Generic, Show, Read)

{-# COMPLETE (:=:), (:≠:), (:≥:), (:>:), (:≤:), (:<:), (:∈:), (:∉:), (:∋:), (:∌:) #-}

pattern (:=:), (:≠:) :: Expr -> Expr -> Rel
pattern e1 :=: e2 = Rel Eq e1 e2
pattern e1 :≠: e2 = Rel Ne e1 e2

pattern  (:≥:), (:>:), (:≤:), (:<:) :: Expr -> Expr -> Rel
pattern e1 :≥: e2 = Rel Ge e1 e2
pattern e1 :>: e2 = Rel Gt e1 e2
pattern e1 :≤: e2 = Rel Le e1 e2
pattern e1 :<: e2 = Rel Lt e1 e2

pattern (:∈:), (:∉:), (:∋:), (:∌:) :: Expr -> Expr -> Rel
pattern e1 :∈: e2 = Rel In e1 e2
pattern e1 :∉: e2 = Rel NotIn e1 e2
pattern e1 :∋: e2 = Rel Ni e1 e2
pattern e1 :∌: e2 = Rel NotNi e1 e2

-- | Matches any relation, discarding the operator.
pattern (:⋈:) :: Expr -> Expr -> Rel
pattern e1 :⋈: e2 <- Rel _ e1 e2

instance Hashable Rel
instance Hashable Rop

instance Uniplate Rel where
  uniplate = plate

instance Biplate Rel Expr where
  biplate (Rel r e1 e2) = plate Rel |- r |* e1 |* e2

instance Pretty Rel where
  pretty p0 = case p0 of
    Rel r p1 p2 -> prettyL p0 p1 <+> pretty r <+> prettyR p0 p2

instance HasFixity Rel where
  fixity _ = Infix NoAss 4

instance Pretty Rop where
  pretty = \case
    Ne -> symNe
    Eq -> symEq
    Le -> symLe
    Lt -> symLt
    Ge -> symGe
    Gt -> symGt
    In -> symIn
    Ni -> symNi
    NotIn -> symNotIn
    NotNi -> symNotNi

instance Subable Rel Expr where
  subst x y = descendBi (subst @Expr x y)  
  freeVars = mconcat . map (freeVars @Expr) . universeBi

------------------------------------------------------------------------------

-- | Returns the left-hand side of a relation.
leftSide :: Rel -> Expr
leftSide (Rel _ e1 _) = e1

-- | Returns the right-hand side of a relation.
rightSide :: Rel -> Expr
rightSide (Rel _ _ e2) = e2

-- | The inverse of a relation, e.g., @a > b@ to @a ≤ b@.
-- Note that this changes the semantics of the relation!
inverse :: Rel -> Rel
inverse = \case
  e1 :=: e2 -> e1 :≠: e2
  e1 :≠: e2 -> e1 :=: e2
  e1 :≥: e2 -> e1 :<: e2
  e1 :≤: e2 -> e1 :>: e2
  e1 :>: e2 -> e1 :≤: e2
  e1 :<: e2 -> e1 :≥: e2
  e1 :∈: e2 -> e1 :∉: e2
  e1 :∉: e2 -> e1 :∈: e2
  e1 :∋: e2 -> e1 :∌: e2
  e1 :∌: e2 -> e1 :∋: e2

-- | The converse of a relation, e.g., @a > b@ to @b < a@. This transformation
-- switches the left and right sides of the relation, while keeping the truth
-- value the same.
converse :: Rel -> Rel
converse = \case
  e1 :=: e2 -> e2 :=: e1
  e1 :≠: e2 -> e2 :≠: e1
  e1 :≥: e2 -> e2 :≤: e1
  e1 :≤: e2 -> e2 :≥: e1
  e1 :>: e2 -> e2 :<: e1
  e1 :<: e2 -> e2 :>: e1
  e1 :∈: e2 -> e2 :∋: e1
  e1 :∉: e2 -> e2 :∌: e1
  e1 :∋: e2 -> e2 :∈: e1
  e1 :∌: e2 -> e2 :∉: e1
  

-- | Whether a relation is an obvious tautology. Note that if this returns
-- 'False', the relation might still be a tautology (just not an obvious one).
isTaut :: Rel -> Bool
isTaut = \case
  e1 :=: e2 -> e1 == e2
  e1 :≤: e2 -> e1 == e2
  e1 :≥: e2 -> e1 == e2
  _         -> False

-- | Whether a relation is an obvious contradiction; see 'isTaut'.
isCont :: Rel -> Bool
isCont = \case
  e1 :≠: e2 -> e1 == e2
  e1 :<: e2 -> e1 == e2
  e1 :>: e2 -> e1 == e2
  _         -> False

------------------------------------------------------------------------------

-- | Normalize a relation and its expressions into a more common/simpler form.
-- Note that this operation does not introduce any abstract values.
normRel :: Rel -> Rel
normRel = rewrite $ \case
  
  ENot e1 :=: ENot e2 -> Just $ e1 :=: e2
  ENot e1 :=: e2      -> Just $ e1 :≠: e2
  e1      :=: ENot e2 -> Just $ e1 :≠: e2
  ENot e1 :≠: ENot e2 -> Just $ e1 :≠: e2
  ENot e1 :≠: e2      -> Just $ e1 :=: e2
  e1      :≠: ENot e2 -> Just $ e1 :=: e2

  IntComp e1 :=: IntComp e2 -> Just $ e1 :=: e2
  IntComp e1 :=: e2         -> Just $ e1 :≠: e2
  e1         :=: IntComp e2 -> Just $ e1 :≠: e2
  IntComp e1 :≠: IntComp e2 -> Just $ e1 :≠: e2

  StrComp e1 :=: StrComp e2 -> Just $ e1 :=: e2
  StrComp e1 :=: e2         -> Just $ e1 :≠: e2
  e1         :=: StrComp e2 -> Just $ e1 :≠: e2
  StrComp e1 :≠: StrComp e2 -> Just $ e1 :≠: e2    

  (StrAt_index s c      ) :=: i -> Just $ EStrAt s (i      ) :=: c
  (StrAt_index s c :+: y) :=: i -> Just $ EStrAt s (i :-: y) :=: c
  (StrAt_index s c :-: y) :=: i -> Just $ EStrAt s (i :+: y) :=: c
  i :=: (StrAt_index s c      ) -> Just $ EStrAt s (i      ) :=: c
  i :=: (StrAt_index s c :+: y) -> Just $ EStrAt s (i :-: y) :=: c
  i :=: (StrAt_index s c :-: y) -> Just $ EStrAt s (i :+: y) :=: c

  (StrAt_index s c      ) :≠: i -> Just $ EStrAt s (i      ) :≠: c
  (StrAt_index s c :+: y) :≠: i -> Just $ EStrAt s (i :-: y) :≠: c
  (StrAt_index s c :-: y) :≠: i -> Just $ EStrAt s (i :+: y) :≠: c
  i :≠: (StrAt_index s c      ) -> Just $ EStrAt s (i      ) :≠: c
  i :≠: (StrAt_index s c :+: y) -> Just $ EStrAt s (i :-: y) :≠: c
  i :≠: (StrAt_index s c :-: y) -> Just $ EStrAt s (i :+: y) :≠: c

  Rel op l1 r1 -> case (normExpr l1, normExpr r1) of 
    (l2,r2) | l1 /= l2 || r1 /= r2 -> Just $ Rel op l2 r2
            | otherwise            -> Nothing
