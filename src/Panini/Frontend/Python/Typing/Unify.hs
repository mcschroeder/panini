{-# LANGUAGE OverloadedLists #-}

{-|
This module implements a solver for Python subtyping constraints, loosely based
on Stephen Dolan's biunification algorithm and followup work.

References:

  * Dolan, Stephen. 2016. "Algebraic Subtyping." PhD diss. University of
    Cambridge.
    https://www.cs.tufts.edu/~nr/cs257/archive/stephen-dolan/thesis.pdf

  * Dolan, Stephen and Alan Mycroft. 2017. "Polymorphism, Subtyping, and Type
    Inference in MLsub." Proceedings of the 44th ACM SIGPLAN Symposium on
    Principles of Programming Languages (POPL '17): 60-72.
    https://doi.org/10.1145/3009837.3009882

  * Parreaux, Lionel. 2020. "The Simple Essence of Algebraic Subtyping:
    Principal Type Inference with Subtyping Made Easy (Functional Pearl)."
    PACMPL 4, ICFP, Article 124 (August 2020). https://doi.org/10.1145/3409006
  
-}
module Panini.Frontend.Python.Typing.Unify (unify) where

import Algebra.Lattice
import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Except
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as Set
import Panini.Frontend.Python.Typing.Monad
import Panini.Frontend.Python.Typing.PyType as PyType
import Panini.Panic
import Prelude

------------------------------------------------------------------------------

unify :: Set Constraint -> Infer (IntMap PyType)
unify = coalesce <=< biunify

------------------------------------------------------------------------------

-- | Coalesce a collection of meta variable bounds into simple static types.
coalesce :: IntMap (PyType, PyType) -> Infer (IntMap PyType)
coalesce = go mempty . IntMap.toList
 where
  go m []             = pure m
  go m ((a,(l,u)):cs) = case metaVars l <> metaVars u of
    [] -> case (l,u) of
      (Any, t)            -> go (IntMap.insert a t  m) cs
      (t, Any)            -> go (IntMap.insert a t  m) cs
      (t1, t2) | t1 == t2 -> go (IntMap.insert a t1 m) cs
      
      (PyType x ts1, PyType y ts2) | x == y -> do
        assertM $ length ts1 == length ts2
        (ts,vs) <- newMetaVars (length ts1)
        let cs' = zip vs (zip ts1 ts2)
        let c' = (a, (PyType x ts, PyType y ts))
        go m $ cs ++ cs' ++ [c']

      (t1, t2) | t1 ⊑ t2   -> go (IntMap.insert a t1 m) cs
               | otherwise -> throwE $ CannotCoalesce a l u

    vs -> go m (cs ++ [(a,(l',u'))])
     where
      l' = IntMap.foldrWithKey' substituteMetaVar l ts
      u' = IntMap.foldrWithKey' substituteMetaVar u ts
      ts = IntMap.restrictKeys m vs <> IntMap.fromSet (const Any) undefs
      undefs = vs IntSet.\\ (IntMap.keysSet m <> IntSet.fromList (map fst cs))
  
  newMetaVars n = do
    ts <- replicateM n newMetaVar
    let unwrap t@(MetaVar v) = (t,v); unwrap _ = impossible
    return $ unzip $ map unwrap ts


-- | Solve subtyping constraints via biunification, returning the lower and
-- upper bounds of each meta variable (cf. Parreaux 2020). Note that these
-- bounds might in turn contain meta variables.
biunify :: Set Constraint -> Infer (IntMap (PyType, PyType))
biunify = go mempty . Set.toList
 where
  go m []     = pure m
  go m (c:cs) = case c of

    Any :≤ _   -> go m cs
    _   :≤ Any -> go m cs

    MetaVar a :≤ t -> go m' cs'
     where
      (l,u) = IntMap.findWithDefault (Any,Any) a m
      u'    = u ⊓ t
      m'    = IntMap.insert a (l,u') m
      cs'   = l :≤ t : cs

    t :≤ MetaVar a -> go m' cs'
     where
      (l,u) = IntMap.findWithDefault (Any,Any) a m
      l'    = l ⊔ t
      m'    = IntMap.insert a (l',u) m
      cs'   = t :≤ u : cs

    Callable s1 t1 :≤ Callable s2 t2 | length s1 == length s2 -> 
      go m $ t1 :≤ t2 : zipWith (:≤) s2 s1 ++ cs

    PyType x ts1 :≤ PyType y ts2 | x == y, length ts1 == length ts2 -> 
      go m $ zipWith (:≤) ts1 ts2 ++ cs
    
    t1 :≤ t2@(PyType y _)
      | [t] <- Set.filter ((y ==) . pyTypeName) $ transitiveSuperTypes t1
      -> go m (t :≤ t2 : cs)
    
    t1 :≤ t2 | t1 ⊑ t2   -> go m cs
             | otherwise -> throwE $ CannotSolve c

    [t1] :*≤ t2 -> go m (t1 :≤ t2 : cs)

    -- TODO: this leads to a huge exponential explosion
    ts :*≤ t -> do
      r <- tryAll $ map (go m . (:cs)) $ map (:≤ t) ts
      case r of
        [] -> throwE $ CannotSolve c
        ms -> pure $ IntMap.unionsWith (\(l1,u1) (l2,u2) -> (l1 ⊔ l2, u1 ⊓ u2)) ms

-- | A 'meet' for Python types that eliminates unknowns ('Any' and 'MetaVar') if
-- possible; returns 'Any' if there is no greatest lower bound.
(⊓) :: PyType -> PyType -> PyType
Any_ _ ⊓ t      = t
t      ⊓ Any_ _ = t
t1     ⊓ t2     = fromMaybe Any (t1 ∧? t2)

-- | A 'join' for Python types that eliminates unknowns ('Any' and 'MetaVar')
(⊔) :: PyType -> PyType -> PyType
Any_ _ ⊔ t      = t
t      ⊔ Any_ _ = t
t1     ⊔ t2     = t1 ∨ t2
