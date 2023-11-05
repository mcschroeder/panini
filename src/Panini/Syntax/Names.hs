-- TODO: module documentation
module Panini.Syntax.Names where

import Data.Char (isDigit)
import Data.Hashable
import Data.String
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Read
import Panini.Pretty
import Panini.Provenance
import Prelude

------------------------------------------------------------------------------

-- | A name for something, e.g. a variable or a binder.
data Name = Name !Text !PV
  deriving stock (Show, Read)

instance HasProvenance Name where
  getPV (Name _ pv) = pv
  setPV pv (Name x _) = Name x pv

-- | Equality between names ignores provenance.
instance Eq Name where
  Name a _ == Name b _ = a == b

-- | Ordering between names ignores provenance.
instance Ord Name where
  Name a _ <= Name b _ = a <= b

-- | Hashing names ignores provenance.
instance Hashable Name where
  hashWithSalt s (Name x _) = hashWithSalt s x
  hash (Name x _) = hash x

instance IsString Name where
  fromString s = Name (Text.pack s) NoPV

instance Pretty Name where
  pretty (Name x _) = ann (Identifier VarIdent) $ pretty x

dummyName :: Name
dummyName = "_"

isDummy :: Name -> Bool
isDummy n = n == dummyName

-- | Returns a fresh name based on a given name, avoiding unwanted names.
freshName :: Foldable t => Name -> t Name -> Name
freshName x ys = go (nextSubscript x)
 where
  go x'
    | x' `elem` ys = go (nextSubscript x')
    | otherwise    = x'

-- | Given "x", returns "x1"; given "x1", returns "x2"; and so on.
nextSubscript :: Name -> Name
nextSubscript (Name n pv) = case decimal @Int $ Text.takeWhileEnd isDigit n of
  Left _      -> Name (Text.append n "1") (Derived pv "rename")
  Right (i,_) -> Name (Text.dropWhileEnd isDigit n <> i') (Derived pv "rename")
                   where i' = Text.pack $ show (i + 1)
