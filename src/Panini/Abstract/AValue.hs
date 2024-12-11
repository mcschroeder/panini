{-# LANGUAGE OverloadedLists #-}
module Panini.Abstract.AValue where

import Algebra.Lattice
import Control.Applicative
import Data.Data (Data)
import Data.Generics.Uniplate.Direct
import Data.Hashable
import Data.Text qualified as Text
import GHC.Generics (Generic)
import Panini.Abstract.ABool as ABool
import Panini.Abstract.AChar as AChar
import Panini.Abstract.AInt as AInt
import Panini.Abstract.AString as AString
import Panini.Abstract.AUnit
import Panini.Pretty
import Panini.Syntax.Primitives
import Panini.Syntax.Relations
import Panini.Syntax.Predicates
import Panini.Syntax.Expressions
import Panini.Solver.Constraints
import Panini.Syntax.Names
import Panini.Syntax.Substitution
import Prelude
import Data.Set ((\\))

------------------------------------------------------------------------------

data AValue
  = AUnit !AUnit
  | ABool !ABool
  | AInt !AInt
  | AChar !AChar
  | AString !AString
  | ARel !Name !Base !RelA
  deriving stock 
    ( Eq
    , Ord  -- ^ structural ordering
    , Show, Read
    , Generic, Data)

instance Hashable AValue

instance Uniplate AValue where
  uniplate = plate

instance Biplate AValue RelA where
  biplate = \case
    AUnit a     -> plate AUnit |- a
    ABool a     -> plate ABool |- a
    AInt a     -> plate AInt |- a
    AChar a     -> plate AChar |- a
    AString a     -> plate AString |- a
    ARel x b r  -> plate ARel |- x |- b |* r

instance Biplate AValue ExprA where
  biplate = \case
    AUnit a     -> plate AUnit |- a
    ABool a     -> plate ABool |- a
    AInt a     -> plate AInt |- a
    AChar a     -> plate AChar |- a
    AString a     -> plate AString |- a
    ARel x b r  -> plate ARel |- x |- b |+ r

instance Pretty AValue where
  pretty = \case
    AUnit   a -> pretty a
    ABool   a -> pretty a
    AInt    a -> pretty a
    AChar   a -> pretty a
    AString a -> pretty a
    ARel x _ r -> braces $ pretty x <> "|" <> pretty r

typeOfAValue :: AValue -> Base
typeOfAValue = \case
  AUnit   _ -> TUnit
  ABool   _ -> TBool
  AInt    _ -> TInt
  AChar   _ -> TChar
  AString _ -> TString
  ARel _ b _ -> b


instance PartialOrder AValue where
  AUnit   a ⊑ AUnit   b = a ⊑ b
  ABool   a ⊑ ABool   b = a ⊑ b
  AInt    a ⊑ AInt    b = a ⊑ b
  AChar   a ⊑ AChar   b = a ⊑ b
  AString a ⊑ AString b = a ⊑ b
  _         ⊑ _         = False

instance PartialMeetSemilattice AValue where
  AUnit   a ∧? AUnit   b = Just $ AUnit   (a ∧ b)
  ABool   a ∧? ABool   b = Just $ ABool   (a ∧ b)
  AInt    a ∧? AInt    b = Just $ AInt    (a ∧ b)
  AChar   a ∧? AChar   b = Just $ AChar   (a ∧ b)
  AString a ∧? AString b = Just $ AString (a ∧ b)
  _         ∧? _         = Nothing

instance PartialJoinSemilattice AValue where
  AUnit   a ∨? AUnit   b = Just $ AUnit   (a ∨ b)
  ABool   a ∨? ABool   b = Just $ ABool   (a ∨ b)
  AInt    a ∨? AInt    b = Just $ AInt    (a ∨ b)
  AChar   a ∨? AChar   b = Just $ AChar   (a ∨ b)
  AString a ∨? AString b = Just $ AString (a ∨ b)
  _         ∨? _         = Nothing

hasTop :: AValue -> Bool
hasTop = \case
  AUnit   a -> isTop a
  ABool   a -> isTop a
  AInt    a -> isTop a
  AChar   a -> isTop a
  AString a -> isTop a
  ARel _ _ _ -> False

hasBot :: AValue -> Bool
hasBot = \case
  AUnit   a -> isBot a
  ABool   a -> isBot a
  AInt    a -> isBot a
  AChar   a -> isBot a
  AString a -> isBot a
  ARel _ _ _ -> False

fillTop :: AValue -> AValue
fillTop = topValue . typeOfAValue

fillBot :: AValue -> AValue
fillBot = botValue . typeOfAValue

topValue :: Base -> AValue
topValue = \case
  TUnit   -> AUnit   top
  TBool   -> ABool   top
  TInt    -> AInt    top
  TChar   -> AChar   top
  TString -> AString top

botValue :: Base -> AValue
botValue = \case
  TUnit   -> AUnit   bot
  TBool   -> ABool   bot
  TInt    -> AInt    bot
  TChar   -> AChar   bot
  TString -> AString bot

fromValue :: Value -> AValue
fromValue = \case
  U   _ -> AUnit Unit
  B b _ -> ABool $ ABool.eq b
  I i _ -> AInt $ AInt.eq i
  C c _ -> AChar $ AChar.eq c
  S t _ -> AString $ AString.eq $ Text.unpack t

------------------------------------------------------------------------------

type RelA  = Rel'  AValue
type ExprA = Expr' AValue
type PredA = Pred' AValue
type ConA  = Con'  AValue

------------------------------------------------------------------------------

instance Uniplate RelA where
  uniplate (Rel op a b) = plate Rel |- op |+ a |+ b

instance Biplate RelA AValue where
  biplate (Rel op a b) = plate Rel |- op |+ a |+ b

instance Subable RelA ExprA where
  subst x y = descendBi (subst @ExprA x y)
  freeVars = mconcat . map (freeVars @ExprA) . childrenBi

-- | The type of a variable in a given relation, if locally discernible.
typeOfVarInRelA :: Name -> RelA -> Maybe Base
typeOfVarInRelA x = \case
  EVar y :=: e      | x == y -> typeOfExprA e
  e      :=: EVar y | x == y -> typeOfExprA e
  EVar y :≠: e      | x == y -> typeOfExprA e
  e      :≠: EVar y | x == y -> typeOfExprA e
  EVar y :<: _      | x == y -> Just TInt
  _      :<: EVar y | x == y -> Just TInt
  EVar y :≤: _      | x == y -> Just TInt
  _      :≤: EVar y | x == y -> Just TInt  
  EVar y :>: _      | x == y -> Just TInt
  _      :>: EVar y | x == y -> Just TInt
  EVar y :≥: _      | x == y -> Just TInt
  _      :≥: EVar y | x == y -> Just TInt  
  EVar y :∈: e      | x == y -> typeOfExprA e
  e      :∈: EVar y | x == y -> typeOfExprA e
  EVar y :∉: e      | x == y -> typeOfExprA e
  e      :∉: EVar y | x == y -> typeOfExprA e  
  Rel _ e1 e2 -> typeOfVarInExpr x e1 <|> typeOfVarInExpr x e2

------------------------------------------------------------------------------

-- ^ abstract value @α@
pattern EAbs :: AValue -> ExprA
pattern EAbs v = EVal v

{-# COMPLETE EVar, EFun, EAbs #-}

-- | abstract unit constant
pattern EUnitA :: AUnit -> ExprA
pattern EUnitA a = EAbs (AUnit a)

-- | abstract Boolean constant
pattern EBoolA :: ABool -> ExprA
pattern EBoolA a = EAbs (ABool a)

-- | abstract integer constant
pattern EIntA :: AInt -> ExprA
pattern EIntA a = EAbs (AInt a)

-- | abstract character constant
pattern ECharA :: AChar -> ExprA
pattern ECharA a = EAbs (AChar a)

-- | abstract string constant
pattern EStrA :: AString -> ExprA
pattern EStrA a = EAbs (AString a)

-- ^ abstract solution @⟨x|r⟩@
pattern ESol :: Name -> Base -> RelA -> ExprA
pattern ESol x b r = EAbs (ARel x b r)

-- | The type of the given expression, if locally discernible.
typeOfExprA :: ExprA -> Maybe Base
typeOfExprA = \case
  EVar _        -> Nothing
  ENot _        -> Just TBool
  _ :+: _       -> Just TInt
  _ :-: _       -> Just TInt
  _ :*: _       -> Just TInt
  EStrLen _     -> Just TInt
  EStrAt _ _    -> Just TChar
  EStrSub _ _ _ -> Just TString
  EStrFirstIndexOfChar _ _ -> Just TInt
  EStrConc _ _ -> Just TString
  EStrStar _ -> Just TString
  EStrContains _ _ -> Just TBool
  EFun _ es     -> asum $ map typeOfExprA es
  EReg _        -> Just TString
  EAbs a        -> Just $ typeOfAValue a

instance Uniplate ExprA where
  uniplate = \case
    EVar x     -> plate EVar |- x
    EFun f es  -> plate EFun |- f ||* es
    EReg r     -> plate EReg |- r
    EAbs a     -> plate EAbs |+ a

instance Biplate ExprA AValue where
  biplate = \case
    EVar x     -> plate EVar |- x
    EFun f es  -> plate EFun |- f ||+ es
    EReg r     -> plate EReg |- r
    EAbs a     -> plate EAbs |* a

instance Biplate ExprA RelA where
  biplate = \case
    EVar x     -> plate EVar |- x
    EFun f es  -> plate EFun |- f ||+ es
    EReg r     -> plate EReg |- r
    EAbs a     -> plate EAbs |+ a

-- see Panini.Syntax.Substitution
instance Subable ExprA ExprA where
  subst x y = \case
    EVar n | y == n -> x
    ESol n b r
      | y == n       -> ESol n  b            r   -- (1)
      | n `freeIn` x -> ESol n' b (subst x y r') -- (2)
      | otherwise    -> ESol n  b (subst x y r ) -- (3)
      where
        r' = subst (EVar n') n r
        n' = freshName n ([y] <> freeVars r)
    
    e -> descend (subst x y) e

  freeVars = \case
    EVar x           -> [x]
    EFun _ es        -> mconcat (map freeVars es)
    EReg _           -> []
    ESol x _ r       -> freeVars r \\ [x]
    EAbs _           -> []
