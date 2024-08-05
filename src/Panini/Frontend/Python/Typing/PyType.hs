{-# LANGUAGE OverloadedLists #-}
module Panini.Frontend.Python.Typing.PyType where

import Data.Data
import Data.Generics.Uniplate.Data
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Set qualified as Set
import Data.String
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
