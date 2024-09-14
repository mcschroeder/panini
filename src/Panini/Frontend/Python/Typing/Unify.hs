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
import Prelude

--import Debug.Trace
--import Panini.Pretty

------------------------------------------------------------------------------

unify :: Set Constraint -> Infer (IntMap PyType)
unify = fmap coalesce . biunify
-- unify cs = do
--   traceM "--------------------"
--   forM_ cs $ traceM . showPretty
--   traceM "--------------------"
--   xs <- biunify cs
--   traceM "--------------------"
--   forM_ (IntMap.toList xs) $ traceM . showPretty
--   traceM "--------------------"
--   let ys = coalesce xs
--   traceM "--------------------"
--   forM_ (IntMap.toList ys) $ traceM . showPretty
--   traceM "--------------------"
--   return ys

------------------------------------------------------------------------------

-- | Coalesce a collection of meta variable bounds into simple static types.
coalesce :: IntMap (Set PyType, Set PyType) -> IntMap PyType
coalesce m0 = go mempty m0 mempty (IntMap.keys m0)
 where
  go f _ _ [] = f    
  go f m s (a:rest) = case IntMap.lookup a m of
  --go f m s (a:rest) = trace (showPretty a) $ case IntMap.lookup a m of
    Nothing -> go f' m' s rest
     where
      f' = IntMap.insert a Any f
      m' = IntMap.insert a ([],[]) m
    Just (l,u) -> case IntSet.toList $ IntSet.unions $ Set.map metaVars $ l <> u of
      [] -> go f' m' s rest
       where
        l1 = case coalesceLower l of [t] -> t; _ -> Any
        u1 = case coalesceUpper u of Just [t] -> t; _ -> Any
        l2 = if l1 == Any then u1 else l1
        u2 = if u1 == Any then l1 else u1
        m' = IntMap.map substVar m
        f' = IntMap.insert a l2 f
        substVar (lx,ux) = ( Set.map (substituteMetaVar a l2) lx
                           , Set.map (substituteMetaVar a u2) ux )
      
      vs | IntSet.member a s -> go (IntMap.insert a Any f) m s rest         
         | otherwise         -> go f m (IntSet.insert a s) (vs ++ a:rest)         

coalesceLower :: Set PyType -> Set PyType
coalesceLower = Set.fromList . go . Set.toList
 where
  go      []        = []
  go (  y:[])       = [y]
  go (x:y:zs)
    | hasMetaVars x = x : go (    y : zs)
    | hasMetaVars y = y : go (x     : zs)
    | otherwise     =     go (x ∨ y : zs)

coalesceUpper :: Set PyType -> Maybe (Set PyType)
coalesceUpper = go mempty . Set.toList
 where
  go m      []         = Just m
  go m (  y:[])        = Just $ Set.insert y m
  go m (x:y:zs)
    | hasMetaVars x    = go (Set.insert x m) (y:zs)
    | hasMetaVars y    = go (Set.insert y m) (x:zs)
    | Just z <- x ∧? y = go               m  (z:zs)
    | otherwise        = Nothing

-- | Solve subtyping constraints via biunification, returning the lower and
-- upper bounds of each meta variable (cf. Parreaux 2020). Note that these
-- bounds might in turn contain meta variables.
biunify :: Set Constraint -> Infer (IntMap (Set PyType, Set PyType))
biunify = go mempty . Set.toList
 where
  go m []     = pure m
  go m (c:cs) = case c of
  --go m (c:cs) = trace (showPretty c) $ case c of

    Any :≤ _   -> go m cs
    _   :≤ Any -> go m cs

    MetaVar a :≤ t -> do
      let (l,u) = IntMap.findWithDefault (mempty,mempty) a m
      case coalesceUpper $ Set.insert t u of
        Nothing -> throwE $ CannotSolve c
        Just u' -> do
          let m'    = IntMap.insert a (l,u') m
          let cs'   = map (:≤ t) (Set.toList l) ++ cs          
          go m' cs'

    t :≤ MetaVar a -> go m' cs'
     where
      (l,u) = IntMap.findWithDefault (mempty,mempty) a m
      l'    = coalesceLower $ Set.insert t l
      m'    = IntMap.insert a (l',u) m
      cs'   = map (t :≤) (Set.toList u) ++ cs

    Callable s1 t1 :≤ Callable s2 t2 | length s1 == length s2 -> 
      go m $ t1 :≤ t2 : zipWith (:≤) s2 s1 ++ cs

    PyType x ts1 :≤ PyType y ts2 | x == y, length ts1 == length ts2 -> 
      go m $ zipWith (:≤) ts1 ts2 ++ cs
    
    -- TODO: generalize
    Str :≤ Iterable t -> go m (Str :≤ t : cs)
    Str :≤ Tuple ts -> go m $ map (Str :≤) ts ++ cs

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
        ms -> do
          let m' = IntMap.unionsWith (\(l1,u1) (l2,u2) -> (l1 <> l2, u1 <> u2)) ms
          fmap IntMap.fromList $ forM (IntMap.toList m') $ \(a,(l,u)) -> do
            let l' = coalesceLower l
            case coalesceUpper u of
              Nothing -> throwE $ CannotSolve c
              Just u' -> return (a, (l',u'))
