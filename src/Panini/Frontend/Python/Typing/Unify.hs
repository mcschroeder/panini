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
module Panini.Frontend.Python.Typing.Unify where

import Algebra.Lattice
import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Except
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.Set (Set)
import Data.Set qualified as Set
import Panini.Frontend.Python.Typing.Monad
import Panini.Frontend.Python.Typing.PyType as PyType
import Prelude

------------------------------------------------------------------------------

unify :: Set Constraint -> Infer (IntMap PyType)
unify = coalesce <=< biunify

------------------------------------------------------------------------------

-- | Coalesce a collection of meta variable bounds into simple static types.
coalesce :: IntMap (PyType, PyType) -> Infer (IntMap PyType)
coalesce = go mempty . IntMap.toList
 where
  go m []               = pure m
  go m ((a,(lo,up)):cs) = case (lo, up, metaVars lo <> metaVars up) of
    (Any, t  , [])            -> go (IntMap.insert a t  m) cs
    (t  , Any, [])            -> go (IntMap.insert a t  m) cs
    (t1 , t2 , []) | t1 == t2 -> go (IntMap.insert a t1 m) cs
    
    -- TODO: generalize
    (Tuple ts1, Tuple ts2, []) | length ts1 == length ts2 -> do
      vs <- replicateM (length ts1) newMetaVar
      let proj (MetaVar i) = i; proj _ = undefined
      let cs' = zip (map proj vs) (zip ts1 ts2)
      let c' = (a,(Tuple vs, Tuple vs))
      go m (cs ++ cs' ++ [c'])

    (_  , _  , [])            -> throwE $ CannotCoalesce a lo up
    (_  , _  , vs)            -> go m (cs ++ [(a,(lower',upper'))])
     where
      lower' = IntMap.foldrWithKey' substituteMetaVar lo finals
      upper' = IntMap.foldrWithKey' substituteMetaVar up finals
      finals = IntMap.restrictKeys m vs <> IntMap.fromSet (const Any) undefs
      undefs = vs IntSet.\\ (IntMap.keysSet m <> IntSet.fromList (map fst cs))

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
    
    Callable s1 t1 :≤ Callable s2 t2 -> 
      go m $ zipWith (:≤) s2 s1 ++ t1 :≤ t2 : cs
    
    MetaVar a :≤ t2 -> go m' (lo :≤ t2 : cs)
     where
      (lo,up) = IntMap.findWithDefault (Any,Any) a m
      m'      = IntMap.insert a (lo, up ∧ t2) m

    t1 :≤ MetaVar a -> go m' (t1 :≤ up : cs)
     where
      (lo,up) = IntMap.findWithDefault (Any,Any) a m
      m'      = IntMap.insert a (lo ∨ t1, up) m

    Union ts :≤ t2 -> do
      r <- diverge m $ map (:≤ t2) ts
      case r of
        [] -> throwE $ CannotUnify (Union ts) t2
        ms -> go (combine ms) cs

    t1 :≤ Union ts -> do
      r <- diverge m $ map (t1 :≤) ts
      case r of
        [] -> throwE $ CannotUnify t1 (Union ts)
        ms -> go (combine ms) cs

    t1 :≤ t2 | hasMetaVars t1 || hasMetaVars t2 -> do
      let st1 = Set.toList $ superTypes t1
      let st2 = Set.toList $ superTypes t2
      r <- diverge m $ zipWith (:≤) st1 st2 ++ map (t1 :≤) st2 ++ map (:≤ t2) st1
      case r of
        [] -> throwE $ CannotUnify t1 t2
        ms -> go (combine ms) cs
      
    t1 :≤ t2 | t1 ⊑ t2   -> go m cs
             | otherwise -> throwE $ CannotUnify t1 t2
  
  diverge m = tryAll . map (go m . pure)
  combine m = IntMap.unionsWith (\(l1,u1) (l2,u2) -> (l1 ∨ l2, u1 ∧ u2)) m
