{-# LANGUAGE OverloadedLists #-}
module Panini.Frontend.Python.Typing.Unify where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Except
import Data.IntSet qualified as IntSet
import Data.Set (Set)
import Data.Set qualified as Set
import Panini.Frontend.Python.Typing.Monad
import Panini.Frontend.Python.Typing.PyType as PyType
import Prelude

------------------------------------------------------------------------------

-- TODO: distinguish between type variable unification and subtyping

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

unify (Tuple as) (Tuple bs) = Tuple <$> zipWithM unify as bs

unify (Callable as b) (Callable cs d) = 
  Callable <$> zipWithM unify as cs <*> unify b d

unify a b
  | b `Set.member` transitiveSuperTypes a = return a
  | a `Set.member` transitiveSuperTypes b = return b
--  | Just c <- commonSuperType a b         = return c
  | otherwise                             = throwE $ CannotUnify a b

------------------------------------------------------------------------------

-- TODO: formulate the Python type hierarchy via some lattice construction
-- and ensure that we have some notion of a least-upper bound, if possible

-- | Returns a common supertype of two types, if it exists, excluding 'Object'.
commonSuperType :: PyType -> PyType -> Maybe PyType
commonSuperType a b
  | not (null cs) = Just $ Union $ Set.toList cs
  | otherwise     = asum $ [commonSuperType a  b' | b' <- Set.toList bs] ++ 
                           [commonSuperType a' b  | a' <- Set.toList as]
 where
  as = superTypes a
  bs = superTypes b
  cs = Set.intersection as bs

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
