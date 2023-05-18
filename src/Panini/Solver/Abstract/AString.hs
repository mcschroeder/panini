-- TODO: replace with proper regular expression domain

module Panini.Solver.Abstract.AString
  ( AString
  , aStringLit
  , aStringRep
  , aStringSigma
  , aStringStar
  ) where

import Prelude
import Panini.Solver.Abstract.AChar
import Panini.Algebra.Lattice
import Panini.Pretty.Printer
import GHC.Generics (Generic)
import Data.Hashable

data AString 
  = Zero                   -- ⊥
  | One                    -- ε
  | Lit AChar              -- c
  | Times AString AString  -- ab
  | Plus AString AString   -- a | b
  | Star AString           -- a*

  | Meet AString AString  -- TODO: remove
  deriving stock (Eq, Show, Read, Generic)

instance Hashable AString

instance Semigroup AString where
  One <> a = a
  a <> One = a
  a <> b = Times a b

instance Monoid AString where
  mempty = One

instance MeetSemilattice AString where
  Zero ∧ _ = Zero
  _ ∧ Zero = Zero
  Star (Lit c) ∧ a | c == (⊤) = a
  a ∧ Star (Lit c) | c == (⊤) = a

  Lit a ∧ Lit b = case a ∧ b of
    c | c == (⊥) -> Zero
    c            -> Lit c

  a ∧ b | a == b = a
  a ∧ b = Meet a b

instance BoundedMeetSemilattice AString where
  (⊤) = Star (Lit (⊤))

instance JoinSemilattice AString where
  Zero ∨ a = a
  a ∨ Zero = a
  a ∨ b | a == b = a
  a ∨ b = Plus a b
  
instance BoundedJoinSemilattice AString where
  (⊥) = Zero

aStringLit :: AChar -> AString
aStringLit = Lit

aStringRep :: AString -> Integer -> AString
aStringRep a n = mconcat $ replicate (fromIntegral n) a

aStringSigma :: AString
aStringSigma = Lit (⊤)

aStringStar :: AString -> AString
aStringStar = Star

instance Pretty AString where
  pretty = \case
    Zero -> "∅"
    One -> "ε"
    Lit c -> pretty c
    Times a b -> pretty a <> pretty b
    Plus a b -> parens (pretty a <+> "|" <+> pretty b)
    Star (Lit c) -> pretty c <> "*"
    Star a -> parens (pretty a) <> "*"
    Meet a b -> pretty a <+> "⊓" <+> pretty b
