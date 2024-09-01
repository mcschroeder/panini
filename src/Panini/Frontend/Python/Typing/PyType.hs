{-# LANGUAGE OverloadedLists #-}
module Panini.Frontend.Python.Typing.PyType where

import Algebra.Lattice
import Data.Data
import Data.Generics.Uniplate.DataOnly
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.List qualified as List
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String
import Panini.Pretty hiding (Set)
import Prelude

-------------------------------------------------------------------------------

data PyType
  = Any_ (Maybe Int)  -- use 'Any' and 'MetaVar' patterns
  | Union_ [PyType]   -- use 'Union' smart constructor
  | TypeVar String  -- TODO: constraints, bound, variance
  | Callable [PyType] (PyType)  

  -- built-in types  
  | Object
  | Int
  | Float
  | Complex
  | Bool
  | List PyType
  | Tuple [PyType]
  | Range
  | Str
  | Bytes
  | Bytearray
  | Memoryview PyType
  | Set PyType
  | Frozenset PyType
  | Dict PyType PyType
  | Slice
  | Enumerate PyType
  | Ellipsis
  | None
  
  -- ABCs (abstract base classes)
  | Container PyType
  | Hashable
  | Iterable PyType
  | Iterator PyType
  | Reversible PyType
  | Generator PyType PyType PyType
  | Sized
  | Collection PyType
  | Sequence PyType
  | MutableSequence PyType
  | AbstractSet PyType
  | MutableSet PyType
  | Mapping PyType PyType
  | MutableMapping PyType PyType
  | MappingView PyType
  | ItemsView PyType PyType
  | KeysView PyType
  | ValuesView PyType
  | Awaitable PyType
  | Coroutine PyType PyType PyType
  | AsyncIterable PyType
  | AsyncIterator PyType
  | AsyncGenerator PyType PyType
  | Buffer

  -- protocols for structural subtyping (duck-typing)
  | SupportsAbs PyType
  | SupportsComplex
  | SupportsDivMod PyType PyType
  | SupportsFloat
  | SupportsInt
  | SupportsTrunc
  | SupportsIndex
  | SupportsIter PyType
  | SupportsNext PyType
  | SupportsRDivMod PyType PyType
  | SupportsRound PyType

  -- miscellaneous
  | NoReturn
  deriving stock (Eq, Ord, Show, Read, Data)

-- | Meta variables are used by the type inference engine to defer inference of
-- unknown types to a later time. They are expected to be eventually unified
-- with some concrete type. From the Python point of view, unresolved meta
-- variables are simply equivalent to 'Any'.
pattern MetaVar :: Int -> PyType
pattern MetaVar i = Any_ (Just i)

pattern Any :: PyType
pattern Any = Any_ Nothing

-- | Union type @Union[X,Y]@ (or @X | Y@)
--
-- This smart constructor behaves similarly to Python's own @typing.Union@ and
-- ensures the following invariants:
--
--  1) Unions of unions are flattened.
--  2) Unions of a single argument vanish.
--  3) Redundant arguments are skipped.
--  4) When comparing unions, the argument order is ignored.
--
-- Note that this constructor will throw an error if the union is empty.
pattern Union :: [PyType] -> PyType
pattern Union xs <- Union_ xs where
  Union xs | null xs'   = error "empty union type"
           | [x] <- xs' = x
           | otherwise  = Union_ xs'
   where
    xs' = Set.toAscList $ Set.fromList $ concatMap flatUnion xs
    flatUnion = \case
      Union ys -> ys
      y            -> [y]

pattern Optional :: PyType -> PyType
pattern Optional t = Union [t, None]

metaVars :: PyType -> IntSet
metaVars t = IntSet.fromList [i | MetaVar i <- universe t]

hasMetaVars :: PyType -> Bool
hasMetaVars t = or [True | MetaVar _ <- universe t]

substituteMetaVar :: Int -> PyType -> PyType -> PyType
substituteMetaVar x t = transform $ \case
  Any_ (Just y) | x == y -> t
  s                      -> s

-------------------------------------------------------------------------------

instance Pretty PyType where
  pretty = \case
    Any_ Nothing  -> "Any"
    Any_ (Just i) -> ann Highlight ("μ" <> pretty i)
    TypeVar s     -> pretty s
    --Union_ ts     -> "Union"      <> params ts
    --Callable xs y -> "Callable["  <> params xs <> "," <> pretty y <> "]"
    Union_ ts     -> prettyFancyUnion ts
    Callable xs y -> prettyFancyCallable xs y
    Tuple ts      -> "tuple"      <> params ts
    List t        -> "list"       <> params [t]
    Memoryview t  -> "memoryview" <> params [t]
    Set t         -> "set"        <> params [t]
    Frozenset t   -> "frozenset"  <> params [t]
    Dict k v      -> "dict"       <> params [k,v]    
    Enumerate t   -> "enumerate"  <> params [t]
    Int           -> "int"
    Float         -> "float"
    Complex       -> "complex"
    Bool          -> "bool"
    Str           -> "str"
    Bytes         -> "bytes"
    Bytearray     -> "bytearray"
    Object        -> "object"
    Slice         -> "slice"
    t             -> prettyConstr t <> paramList (gmapQ paramQ t)
   where
    params       = paramList . map pretty
    paramList [] = mempty
    paramList ts = "[" <> mconcat (List.intersperse "," ts) <> "]"    
    prettyConstr = pretty . showConstr . toConstr    
    paramQ t     = maybe undefined (pretty @PyType) (cast t)

-- | Callable type syntax based on /rejected/ PEP 677.
prettyFancyCallable :: [PyType] -> PyType -> Doc
prettyFancyCallable xs y = 
  parens (mconcat $ List.intersperse ", " $ map pretty xs) <+> "->" <+> pretty y

-- | Union type syntax based on PEP 604, official syntax as of Python 3.10.
prettyFancyUnion :: [PyType] -> Doc
prettyFancyUnion ts = mconcat $ List.intersperse " | " $ map pretty ts

-------------------------------------------------------------------------------

-- | The Python type hierarchy establishes a partial ordering of Python types.
instance PartialOrder PyType where
  Any ⊑ _        = True
  _   ⊑ Any      = True
  _   ⊑ Object   = True
  a   ⊑ Union bs = any (a ⊑) bs
  a   ⊑ b        = a == b || b `elem` transitiveSuperTypes a

instance JoinSemilattice PyType where
  Any ∨ b = b
  a ∨ Any = a
  a ∨ b | a ⊑ b = b
        | b ⊑ a = a
        | otherwise = Union [a,b]

instance MeetSemilattice PyType where
  Any ∧ b = b
  a ∧ Any = a
  a ∧ b | a ⊑ b = a
        | b ⊑ a = b
        | otherwise = Any

------------------------------------------------------------------------------

-- | Return all transitive supertypes of a 'PyType', excluding 'Object'.
transitiveSuperTypes :: PyType -> Set PyType
transitiveSuperTypes = go . superTypes
 where
  go [] = []
  go xs = xs <> go (Set.unions $ Set.map superTypes xs)

-- | Return the immediate supertypes of a 'PyType', excluding 'Object' but
-- including any abstract base classes and duck-typing protocols.
superTypes :: PyType -> Set PyType
superTypes = \case
  Int -> 
    [ SupportsInt, SupportsIndex, SupportsFloat, SupportsTrunc
    , SupportsAbs Int, SupportsDivMod Int Int, SupportsRDivMod Int Int
    , SupportsRound Int
    ]
  Float -> 
      [ SupportsInt, SupportsFloat, SupportsTrunc, SupportsAbs Float
      , SupportsDivMod Float Float, SupportsRDivMod Float Float
      , SupportsRound Int, SupportsRound Float
      ]
  Complex -> [SupportsComplex, SupportsAbs Float]  
  Bool  -> [Int]   
  
  Str          -> [Sequence Str]
  Bytes        -> [Sequence Int]
  Bytearray    -> [MutableSequence Int]  
  Memoryview i -> [Sequence i]
  List a       -> [MutableSequence a]
  Dict k v     -> [MutableMapping k v]
  Set a        -> [MutableSet a]
  Frozenset a  -> [AbstractSet a]
  Enumerate a  -> [Iterator (Tuple [Int,a])]
  Range        -> [Sequence Int]
  
  Tuple (t:ts) 
    | all (== t) ts -> [Sequence t]
    | otherwise     -> [Sequence Any]

  AbstractSet a       -> [Collection a]
  AsyncGenerator y _  -> [AsyncIterator y]  
  AsyncIterator a     -> [AsyncIterable a]
  Collection a        -> [Sized, Iterable a, Container a]
  Coroutine _ _ r     -> [Awaitable r]
  Generator y _ _     -> [Iterator y]
  ItemsView k v       -> [MappingView (Tuple [k,v]), AbstractSet (Tuple [k,v])]
  Iterable a          -> [SupportsIter a]
  Iterator a          -> [SupportsNext a, Iterable a]
  KeysView k          -> [MappingView k, AbstractSet k]
  Mapping k _         -> [Collection k]
  MappingView _       -> [Sized]
  MutableMapping  k v -> [Mapping k v]
  MutableSequence a   -> [Sequence a]
  MutableSet a        -> [AbstractSet a]
  Reversible a        -> [Iterable a]
  Sequence a          -> [Reversible a, Collection a]
  ValuesView v        -> [MappingView v, Collection v]
  
  _ -> []
