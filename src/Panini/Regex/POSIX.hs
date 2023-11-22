module Panini.Regex.POSIX where

import Panini.Regex.CharSet (CharSet(..))
import Panini.Regex.CharSet qualified as CS
import Panini.Regex.Type as Regex
import Prelude

-- TODO: print character ranges
-- TODO: print pre-defined POSIX character classes
-- TODO: proper escaping
-- TODO: parse EREs
-- TODO: Regex AnyChar vs ERE period: latter does not match \NUL !

-------------------------------------------------------------------------------

-- | POSIX Extended Regular Expression
data ERE
  = Per           -- ^ period @.@ (matches anything except @\NUL@)
  | Chr Char      -- ^ single ordinary character @a@
  | Bra BE        -- ^ bracket expression @[abc]@, @[a-z]@, @[^abc]
  | Grp ERE       -- ^ group @(r)@
  | Ast ERE       -- ^ asterisk @*@ (matches zero or more expressions)
  | Pls ERE       -- ^ plus sign @+@ (matches one or more expressions)
  | Que ERE       -- ^ question mark @?@ (matches zero or one expressions)
  | Dup ERE Iva   -- ^ duplication with interval @r{m,n}@
  | Alt ERE ERE   -- ^ alternation @x|y@
  | Con ERE ERE   -- ^ concatenation @xy@
  deriving stock (Show, Read, Eq, Ord)

-- | Duplication interval
data Iva
  = Exa Int      -- ^ exact number of occurrences @{m}@
  | Min Int      -- ^ minimum number of occurrences @{m,}@
  | Bet Int Int  -- ^ number of occurrences in interval @{m,n}@
  deriving stock (Show, Read, Eq, Ord)

-- | Bracket Expression
data BE
  = Mat [BET]  -- ^ matching expression @[abc]@
  | Non [BET]  -- ^ non-matching expression @[^abc]
  deriving stock (Show, Read, Eq, Ord)

-- | Bracket expression term
data BET 
  = Ord Char       -- ^ ordinary character @a@
  | Ran Char Char  -- ^ range of consecutive characters @a-z@
  -- | Col -- ^ collating symbol @[.ch.]@
  -- | Cls -- ^ character class @[:alpha:]@
  -- | Equ -- ^ equivalence class @[=a=]@
  deriving stock (Show, Read, Eq, Ord)

-------------------------------------------------------------------------------

printERE :: ERE -> String
printERE = \case
  Per             -> "."
  Chr c           -> [c]  -- TODO: escaping
  Bra b           -> printBE b
  Grp r           -> "(" <> printERE r <> ")"
  Ast r           -> printERE r <> "*"
  Pls r           -> printERE r <> "+"
  Que r           -> printERE r <> "?"
  Dup r (Exa m)   -> printERE r <> "{" <> show m <> "}"
  Dup r (Min m)   -> printERE r <> "{" <> show m <> ",}"
  Dup r (Bet m n) -> printERE r <> "{" <> show m <> "," <> show n <> "}"
  Alt r1 r2       -> printERE r1 <> "|" <> printERE r2
  Con r1 r2       -> printERE r1 <> printERE r2

-- TODO: escaping
printBE :: BE -> String
printBE = \case
  Mat xs -> "["  <> mconcat (map go xs) <> "]"
  Non xs -> "[^" <> mconcat (map go xs) <> "]"
 where  
  go = \case
    Ord c     -> [c]
    Ran c1 c2 -> c1:'-':c2:[]

-------------------------------------------------------------------------------

toERE :: Regex -> ERE
toERE = go False
 where
  go grouped = \case
    Zero          -> Bra (Mat [])  -- TODO: hack: this is illegal in POSIX
    One           -> undefined
    AnyChar       -> Per  -- TODO: mismatch due to \NUL !
    Lit (CharSet b s) -> case (b, CS.intSetToCharList s) of
      (True , [x]) -> Chr x
      (True , xs ) -> Bra $ Mat $ map Ord xs
      (False, xs ) -> Bra $ Non $ map Ord xs
      -- TODO: recognize range expressions
      -- TODO: recognize character classes?
    Word [c]      -> Chr c
    Word s        -> foldr1 Con $ map Chr s
    Plus xs 
      | grouped   -> Grp $ foldr1 Alt $ map (go False) xs
      | otherwise -> foldr1 Alt $ map (go False) xs
    Times xs      -> foldr1 Con $ map (go True) xs
    Star r        -> Ast $ go True r
    Opt r         -> Que $ go True r
