{-# OPTIONS_GHC -Wno-missing-pattern-synonym-signatures #-}
{-# OPTIONS_GHC -Wno-missing-exported-pattern-synonym-signatures #-}
{-# LANGUAGE OverloadedLists #-}
module Panini.Frontend.Python.Typing.PyType where

import Algebra.Lattice
import Data.Data
import Data.Generics.Uniplate.Direct
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.List qualified as List
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String
import Panini.Pretty hiding (Set)
import Prelude

-------------------------------------------------------------------------------

-- | Type of Python types. 
--
-- Most Python types are represented using the 'PyType' constructor, which takes
-- the name of the type and a (possibly empty) list of type parameters. For
-- convenience and safety, we provide pattern synonyms for most built-in types.
-- For example, the 'Str' pattern translates to @PyType "str" []@, which
-- represents the Python @str@ type; the pattern application @'List' 'Int'@
-- translates to @PyType "list" [PyType "int" []]@ and represents @list[int]@.
--
-- The 'Union' pattern is special: it is a smart constructor that additionally
-- preserves certain invariants of Python type unions and ensures a unique
-- representation.
--
-- The 'Any' and 'MetaVar' patterns translate to the 'Any_' constructor, which
-- should not be used directly and represents both the Python @Any@ type and
-- type system internal meta variables.
--
-- The 'Callable' constructor directly represents the Python @Callable@ type,
-- equivalent to a function type. 
--
-- Finally, the 'TypeVar' constructor represents named Python type variables.
data PyType
  = PyType String [PyType]  -- ^ some indexed type; use patterns where possible
  | Any_ (Maybe Int)         -- ^ use 'Any' and 'MetaVar' patterns
  | Callable [PyType] (PyType)
  | TypeVar String -- TODO: constraints, bound, variance  
  deriving stock (Eq, Ord, Show, Read, Data)

instance Uniplate PyType where
  uniplate = \case
    Any_ v        -> plate Any_ |- v
    TypeVar x     -> plate TypeVar |- x
    Callable xs y -> plate Callable ||* xs |* y
    Union ts      -> plate Union ||* ts  -- IMPORTANT: preserves invariants    
    PyType x ts   -> plate PyType |- x ||* ts

instance Pretty PyType where
  pretty = \case
    Any_ Nothing  -> "Any"
    Any_ (Just i) -> ann Highlight ("μ" <> pretty i)
    TypeVar s     -> pretty s
    --Callable xs y -> "Callable["  <> params xs <> "," <> pretty y <> "]"
    Callable xs y -> prettyFancyCallable xs y
    Union ts      -> prettyFancyUnion ts
    PyType x ts   -> pretty x <> params ts
   where
    params       = paramList . map pretty
    paramList [] = mempty
    paramList ts = "[" <> mconcat (List.intersperse "," ts) <> "]"    

-- | Callable type syntax based on /rejected/ PEP 677.
prettyFancyCallable :: [PyType] -> PyType -> Doc
prettyFancyCallable xs y = 
  parens (mconcat $ List.intersperse ", " $ map pretty xs) <+> "->" <+> pretty y

-- | Union type syntax based on PEP 604, official syntax as of Python 3.10.
prettyFancyUnion :: [PyType] -> Doc
prettyFancyUnion ts = mconcat $ List.intersperse " | " $ map pretty ts

-------------------------------------------------------------------------------

pattern Any :: PyType
pattern Any = Any_ Nothing

-- | Meta variables are used by the type inference engine to defer inference of
-- unknown types to a later time. They are expected to be eventually unified
-- with some concrete type. From the Python point of view, unresolved meta
-- variables are simply equivalent to 'Any'.
pattern MetaVar :: Int -> PyType
pattern MetaVar i = Any_ (Just i)

metaVars :: PyType -> IntSet
metaVars t = IntSet.fromList [i | MetaVar i <- universe t]

hasMetaVars :: PyType -> Bool
hasMetaVars t = or [True | MetaVar _ <- universe t]

substituteMetaVar :: Int -> PyType -> PyType -> PyType
substituteMetaVar x t = transform $ \case
  Any_ (Just y) | x == y -> t
  s                      -> s

-------------------------------------------------------------------------------

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
pattern Union xs <- PyType "Union" xs where
  Union xs | null xs'   = error "empty union type"
           | [x] <- xs' = x
           | otherwise  = PyType "Union" xs'
   where
    xs' = Set.toAscList $ Set.fromList $ concatMap flatUnion xs
    flatUnion = \case
      Union ys -> ys
      y        -> [y]

pattern Optional :: PyType -> PyType
pattern Optional t = Union [t, None]

-------------------------------------------------------------------------------

-- built-in types  
pattern Object                = PyType "object" []
pattern Int                   = PyType "int" []
pattern Float                 = PyType "float" []
pattern Complex               = PyType "complex" []
pattern Bool                  = PyType "bool" []
pattern List t                = PyType "list" [t]
pattern Tuple ts              = PyType "tuple" ts
pattern Range                 = PyType "range" []
pattern Str                   = PyType "str" []
pattern Bytes                 = PyType "bytes" []
pattern Bytearray             = PyType "bytearray" []
pattern Memoryview t          = PyType "memoryview" [t]
pattern Set t                 = PyType "set" [t]
pattern Frozenset t           = PyType "frozenset" [t]
pattern Dict k v              = PyType "dict" [k,v]
pattern Slice                 = PyType "slice" []
pattern Enumerate t           = PyType "enumerate" [t]
pattern Ellipsis              = PyType "Ellipsis" []
pattern None                  = PyType "None" []

-- ABCs (abstract base classes)
pattern Container t           = PyType "Container" [t]
pattern Hashable              = PyType "Hashable" []
pattern Iterable t            = PyType "Iterable" [t]
pattern Iterator t            = PyType "Iterator" [t]
pattern Reversible t          = PyType "Reversible" [t]
pattern Generator y s r       = PyType "Generator" [y,s,r]
pattern Sized                 = PyType "Sized" []
pattern Collection t          = PyType "Collection" [t]
pattern Sequence t            = PyType "Sequence" [t]
pattern MutableSequence t     = PyType "MutableSequence" [t]
pattern AbstractSet t         = PyType "AbstractSet" [t]
pattern MutableSet t          = PyType "MutableSet" [t]
pattern Mapping k v           = PyType "Mapping" [k,v]
pattern MutableMapping k v    = PyType "MutableMapping" [k,v]
pattern MappingView t         = PyType "MappingView" [t]
pattern ItemsView k v         = PyType "ItemsView" [k,v]
pattern KeysView t            = PyType "KeysView" [t]
pattern ValuesView t          = PyType "ValuesView" [t]
pattern Awaitable t           = PyType "Awaitable" [t]
pattern Coroutine y s r       = PyType "Coroutine" [y,s,r]
pattern AsyncIterable t       = PyType "AsyncIterable" [t]
pattern AsyncIterator t       = PyType "AsyncIterator" [t]
pattern AsyncGenerator y s    = PyType "AsyncGenerator" [y,s]
pattern Buffer                = PyType "Buffer" []

-- protocols for structural subtyping (duck-typing)
pattern SupportsAbs t         = PyType "SupportsAbs" [t]
pattern SupportsComplex       = PyType "SupportsComplex" []
pattern SupportsDivMod t r    = PyType "SupportsDivMod" [t,r]
pattern SupportsFloat         = PyType "SupportsFloat" []
pattern SupportsInt           = PyType "SupportsInt" []
pattern SupportsTrunc         = PyType "SupportsTrunc" []
pattern SupportsIndex         = PyType "SupportsIndex" []
pattern SupportsIter t        = PyType "SupportsIter" [t]
pattern SupportsNext t        = PyType "SupportsNext" [t]
pattern SupportsRDivMod t r   = PyType "SupportsRDivMod" [t,r]
pattern SupportsRound t       = PyType "SupportsRound" [t]

-- miscellaneous
pattern NoReturn              = PyType "NoReturn" []

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
