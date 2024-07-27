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
  | TypeVar String  -- TODO: constraints, bound, variance
  | Union_ [PyType]   -- use 'Union' smart constructor
  | Callable [PyType] (PyType)
  | None
  | Bool
  | Int
  | Float
  | Complex
  | Str
  | Bytes
  | Bytearray
  | Object
  | List PyType
  | Dict PyType PyType
  | Tuple [PyType]
  | Iterable PyType  
  | Sequence PyType
  | Collection PyType
  | Mapping PyType PyType
  | Number
  | Buffer
  | AnyStr
  | Hashable
  | SupportsAbs PyType
  | SupportsRound PyType
  | SupportsIndex
  | SupportsComplex
  | SupportsFloat  
  | AsyncIterable PyType
  | AsyncIterator PyType
  | Slice
  | Ellipsis
  | Set PyType
  | ConvertibleToInt
  | ConvertibleToFloat
  | ReadableBuffer
  | Iterator PyType
  | SupportsDivMod PyType PyType
  | SupportsRDivMod PyType PyType
  | NoReturn
  | SupportsIter PyType
  | SupportsNext PyType
  | Sized
  | Reversible
  | MutableSequence PyType
  | MutableSet PyType
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
