{-# LANGUAGE OverloadedLists #-}
module Panini.Frontend.Python.Typing.Unify where

import Control.Monad
import Control.Monad.Trans.Except
import Data.IntSet qualified as IntSet
import Data.Set (Set)
import Data.Set qualified as Set
import Panini.Frontend.Python.Typing.Monad
import Panini.Frontend.Python.Typing.PyType as PyType
import Prelude

------------------------------------------------------------------------------

unify :: PyType -> PyType -> Infer PyType
unify a b | a == b = return a

unify Any b = return b
unify a Any = return a

unify Object b = return b
unify a Object = return a

unify t1@(MetaVar a) b = do
  when (a `IntSet.member` metaVars b) $ throwE $ InfiniteType t1 b
  t <- maybe (return b) (unify b) =<< lookupMetaVar a
  setMetaVar a t
  return t

unify a b@(MetaVar _) = unify b a

unify t1@(Union as) b = do
  cs <- tryAll $ map (unify b) as
  when (null cs) $ throwE $ CannotUnify t1 b
  return (Union cs)

unify a b@(Union _) = unify b a

unify (Callable as b) (Callable cs d) = 
  Callable <$> zipWithM unify as cs <*> unify b d

unify a b = do
  as <- tryAll $ map (unify b) (Set.toList $ superTypes a)
  bs <- tryAll $ map (unify a) (Set.toList $ superTypes b)
  case (null as, null bs) of
    (False , _    ) -> return a
    (_     , False) -> return b
    (True  , True ) -> throwE $ CannotUnify a b

------------------------------------------------------------------------------

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
  PyType.Set a -> [MutableSet a]
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
