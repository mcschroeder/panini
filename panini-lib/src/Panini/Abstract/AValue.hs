{-# LANGUAGE OverloadedLists #-}
module Panini.Abstract.AValue where

import Algebra.Lattice
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
  | ARel !Name !Base !ARel
  deriving stock 
    ( Eq
    , Ord  -- ^ structural ordering
    , Show, Read
    , Generic, Data)

instance Hashable AValue

instance Uniplate AValue where
  uniplate = plate

instance Biplate AValue ARel where
  biplate = \case
    AUnit a     -> plate AUnit |- a
    ABool a     -> plate ABool |- a
    AInt a     -> plate AInt |- a
    AChar a     -> plate AChar |- a
    AString a     -> plate AString |- a
    ARel x b r  -> plate ARel |- x |- b |* r

instance Biplate AValue AExpr where
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
    ARel x _ r -> mangles $ pretty x <> colon <+> pretty r

typeOfAValue :: AValue -> Base
typeOfAValue = \case
  AUnit   _ -> TUnit
  ABool   _ -> TBool
  AInt    _ -> TInt
  AChar   _ -> TChar
  AString _ -> TString
  ARel _ b _ -> b

groundValue :: AValue -> Bool
groundValue (ARel _ _ _) = False
groundValue _            = True

instance PartialOrder AValue where
  AUnit   a ⊑ AUnit   b = a ⊑ b
  ABool   a ⊑ ABool   b = a ⊑ b
  AInt    a ⊑ AInt    b = a ⊑ b
  AChar   a ⊑ AChar   b = a ⊑ b
  AString a ⊑ AString b = a ⊑ b
  a         ⊑ b         = if a == b then True else False

instance PartialMeetSemilattice AValue where
  AUnit   a ∧? AUnit   b = Just $ AUnit   (a ∧ b)
  ABool   a ∧? ABool   b = Just $ ABool   (a ∧ b)
  AInt    a ∧? AInt    b = Just $ AInt    (a ∧ b)
  AChar   a ∧? AChar   b = Just $ AChar   (a ∧ b)
  AString a ∧? AString b = Just $ AString (a ∧ b)
  a         ∧? b         = if a == b then Just a else Nothing

instance PartialJoinSemilattice AValue where
  AUnit   a ∨? AUnit   b = Just $ AUnit   (a ∨ b)
  ABool   a ∨? ABool   b = Just $ ABool   (a ∨ b)
  AInt    a ∨? AInt    b = Just $ AInt    (a ∨ b)
  AChar   a ∨? AChar   b = Just $ AChar   (a ∨ b)
  AString a ∨? AString b = Just $ AString (a ∨ b)
  a         ∨? b         = if a == b then Just a else Nothing

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

isSingleton :: AValue -> Bool
isSingleton = \case
  AUnit   a | Unit       <- a              -> True
  ABool   a | Just _     <- ABool.value  a -> True
  AInt    a | [_]        <- AInt.values  a -> True
  AChar   a | [_]        <- AChar.values a -> True
  AString a | AString1 _ <- a              -> True
  _                                        -> False

------------------------------------------------------------------------------

type ARel  = Rel'  AValue
type AExpr = Expr' AValue
type APred = Pred' AValue
type ACon  = Con'  AValue

------------------------------------------------------------------------------

instance Uniplate ARel where
  uniplate (Rel op a b) = plate Rel |- op |+ a |+ b

instance Biplate ARel AExpr where
  biplate (Rel op a b) = plate Rel |- op |* a |* b

instance Biplate ARel AValue where
  biplate (Rel op a b) = plate Rel |- op |+ a |+ b

instance Subable ARel AExpr where
  subst x y = descendBi (subst @AExpr x y)
  freeVars = mconcat . map (freeVars @AExpr) . childrenBi

instance {-# OVERLAPPING #-} Pretty ARel where
  pretty = \case
    Rel Eq a b -> pretty a <+> "≬" <+> pretty b
    Rel Ne a b -> pretty a <+> "∥" <+> pretty b
    Rel op a b -> pretty a <+> pretty op <+> pretty b

-- | How often does a particular variable occur free in the given relation?
occurrences :: Name -> ARel -> Int
occurrences x ρ = sum [1 | EVar y _ <- universeBi @ARel @AExpr ρ, y == x]

------------------------------------------------------------------------------

-- ^ abstract value @α@
pattern EAbs :: AValue -> AExpr
pattern EAbs v = EVal v

{-# COMPLETE EVar, EFun, EAbs #-}

-- | abstract unit constant
pattern EUnitA :: AUnit -> AExpr
pattern EUnitA a = EAbs (AUnit a)

-- | abstract Boolean constant
pattern EBoolA :: ABool -> AExpr
pattern EBoolA a = EAbs (ABool a)

-- | abstract integer constant
pattern EIntA :: AInt -> AExpr
pattern EIntA a = EAbs (AInt a)

-- | abstract character constant
pattern ECharA :: AChar -> AExpr
pattern ECharA a = EAbs (AChar a)

-- | abstract string constant
pattern EStrA :: AString -> AExpr
pattern EStrA a = EAbs (AString a)

-- ^ abstract relation @⟨x: ρ⟩@
pattern ERelA :: Name -> Base -> ARel -> AExpr
pattern ERelA x b r = EAbs (ARel x b r)

-- | 'True' if all abstract values of in the expression are singletons.
concreteish :: AExpr -> Bool
concreteish ω = and [isSingleton â | EVal â <- universe ω]

-- | Whether an abstract expression contains any bottoms.
anyBot :: AExpr -> Bool
anyBot ω = any hasBot [â | EVal â <- universe ω]

-- | The type of the given expression, if locally discernible.
typeOfExprA :: AExpr -> Maybe Base
typeOfExprA = \case
  EVar _ b      -> Just b
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
  EFun _ _      -> Nothing
  EReg _        -> Just TString
  EAbs a        -> Just $ typeOfAValue a

instance Uniplate AExpr where
  uniplate = \case
    EVar x b   -> plate EVar |- x |- b
    EFun f es  -> plate EFun |- f ||* es
    EReg r     -> plate EReg |- r
    EAbs a     -> plate EAbs |+ a

instance Biplate AExpr AValue where
  biplate = \case
    EVar x b   -> plate EVar |- x |- b
    EFun f es  -> plate EFun |- f ||+ es
    EReg r     -> plate EReg |- r
    EAbs a     -> plate EAbs |* a

instance Biplate AExpr ARel where
  biplate = \case
    EVar x b   -> plate EVar |- x |- b
    EFun f es  -> plate EFun |- f ||+ es
    EReg r     -> plate EReg |- r
    EAbs a     -> plate EAbs |+ a

-- see Panini.Syntax.Substitution
instance Subable AExpr AExpr where
  subst x y = \case
    EVar n _ | y == n -> x
    ERelA n b r
      | y == n       -> ERelA n  b            r   -- (1)
      | n `freeIn` x -> ERelA n' b (subst x y r') -- (2)
      | otherwise    -> ERelA n  b (subst x y r ) -- (3)
      where
        r' = subst (EVar n' b) n r
        n' = freshName n ([y] <> freeVars r)
    
    e -> descend (subst x y) e

  freeVars = \case
    EVar x _         -> [x]
    EFun _ es        -> mconcat (map freeVars es)
    EReg _           -> []
    ERelA x _ r       -> freeVars r \\ [x]
    EAbs _           -> []
