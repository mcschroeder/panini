{-# LANGUAGE RecordWildCards #-}

{-|
This module is concerned with the dominance relation between nodes in a
control-flow graph. It contains low-level functions to compute the immediate
dominators and the dominance frontier of all vertices in an arbitrary flowgraph,
as well as some high-level wrappers to work with Python CFGs.

References:

  * Cytron, Ron, Jeanne Ferrante, Barry K. Rosen, Mark N. Wegman, F. Kenneth
    Zadeck. 1991. "Efficiently Computing Static Single Assignment Form and the
    Control Dependence Graph." TOPLAS 13, no. 4 (October 1991): 451-490.
    https://doi.org/10.1145/115372.115320

  * Lengauer, Thomas and Robert Endre Tarjan. 1979. "A Fast Algorithm for
    Finding Dominators in a Flowgraph." TOPLAS 1, no. 1 (July 1979): 121-141.
    https://doi.org/10.1145/357062.357071

-}
module Panini.Frontend.Python.Dom where

import Control.Monad.Extra
import Control.Monad.ST
import Data.Array.ST
import Data.Array.Unboxed
import Data.Bifunctor
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.List qualified as List
import Data.STRef
import Panini.Frontend.Python.CFG
import Panini.Panic
import Prelude hiding (succ,pred)

------------------------------------------------------------------------------

-- | The dominator tree summarizes the dominance relations between nodes in a
-- control-flow graph.
data DomTree = DomTree
  { domChildren :: IntMap [Label]
  , domFrontier :: IntMap [Label]
  , domTreeRoot :: Label
  }
  deriving stock (Show)

------------------------------------------------------------------------------

-- | Compute the dominator tree of a CFG, including all dominance frontiers.
domTree :: CFG -> DomTree
domTree cfg = DomTree{..}
 where
  n         = IntMap.size cfg.nodeMap
  labels    = IntMap.keys cfg.nodeMap
  vertices  = [1..n]
  labelMap  = IntMap.fromList $ zip labels vertices
  vertexMap = IntMap.fromList $ zip vertices labels
  toVertex  = (IntMap.!) labelMap
  toLabel   = (IntMap.!) vertexMap
  getNode   = (IntMap.!) cfg.nodeMap  

  successors' = map toVertex . successors . getNode . toLabel
  
  r    = toVertex cfg.entry
  succ = listArray (1,n) (map successors' vertices)
  idom = dominators succ r
  tree = dominatorTree idom
  df   = dominanceFrontiers succ tree idom
  
  toLabelAssocs = map (bimap toLabel (map toLabel))

  domChildren = IntMap.fromList $ toLabelAssocs $ tail $ assocs tree
  domFrontier = IntMap.fromList $ toLabelAssocs $ assocs df
  domTreeRoot = r

------------------------------------------------------------------------------

type Vertex = Int
type VertexSet = [Int]  -- TODO: use IntSet

-- | Calculate the distance frontiers for each vertex in a flowgraph.
--
-- This function requires three inputs:
--
--   1. The successors in the flowgraph (same as the input to 'dominators').
--   2. The dominator tree of the flowgraph (as returned by 'dominatorTree').
--   3. The immediate dominators of all vertices (as returned by 'dominators').
--
-- The output array maps each vertex X of the input graph to a set of all
-- flowgraph vertices Y such that X dominates a predecessor of Y but does not
-- strictly dominate Y.
--
-- This is an implementation of the algorithm by Cytron et al. (1991, Fig. 10).
-- The runtime is O(|E|+|DF|), where E is the set of edges in the input graph
-- and DF the result mapping of dominance frontiers. In the worst case, this
-- amounts to O(|E|+|V|^2), where V is the set of all vertices in the input
-- graph; however, in practice, the size of DF is usually linear.
dominanceFrontiers 
  :: Array Vertex VertexSet  -- ^ successors in the flowgraph
  -> Array Vertex VertexSet  -- ^ children in the dominator tree
  -> UArray Vertex Vertex    -- ^ immediate dominators
  -> Array Vertex VertexSet  -- ^ dominance frontiers
dominanceFrontiers succ children idom = runSTArray $ do
  let (m,n) = bounds succ
  assertM (m == 1)
  df <- newArray (1,n) []
  ---------------------------------------------------------
  let
    go x = do
      mapM_ go (children ! x)
      forM_ (succ ! x) $ \y ->
        when (idom ! y /= x) $ modifyArray' df x (y:)
      forM_ (children ! x) $ \z -> do
        ys <- readArray df z
        forM_ ys $ \y ->
          when (idom ! y /= x) $ modifyArray' df x (y:)
  ---------------------------------------------------------
  assertM (length (children ! 0) == 1)
  go $ head $ children ! 0
  return df

-- | Compute the dominator tree from an array of immediate dominators.
--
-- The input array is a map of vertices to their immediate dominators, as
-- computed by the 'dominators' function below. In particular, the root vertex
-- has an immediate dominator of 0, which is outside the valid vertex range.
--
-- The output tree is arranged as another array, which maps parent vertices to
-- sets of child vertices, such that the children of a vertex are all
-- immediately dominated by that vertex. Note that this array starts at index 0,
-- which maps to the root of the tree.
dominatorTree :: UArray Vertex Vertex -> Array Vertex VertexSet
dominatorTree idom =
  accumArray (flip (:)) [] (0,n) $ map (\(v,d) -> (d,v)) $ assocs idom
 where
  (_,n) = bounds idom

-- | Compute all immediate dominators in a flowgraph.
--
-- The input graph is given as an array of sets of (control-flow) successor
-- vertices, together with an initial vertex. Note that the vertices of the
-- graph are expected to be a contiguous range of integers, starting at 1.
--
-- The output array maps each vertex of the input graph to its immediate
-- dominator. Note that since the root vertex has no immediate dominator, it is
-- instead mapped to 0, which is outside the range of input vertices.
--
-- This is an implementation of the "sophisticated" version of the
-- Lengauer-Tarjan algorithm (1979) and closely follows their exposition in the
-- paper. The runtime is O(|E|*α(|E|,|V|)), where E and V are the sets of edges
-- and vertices in the input graph, and α is the functional inverse of
-- Ackerman's function.
dominators :: Array Vertex VertexSet -> Vertex -> UArray Vertex Vertex
dominators succ r = runSTUArray $ do
  let (m,n) = bounds succ
  assertM (m == 1)

  parent   <- newArray (1,n) 0  :: ST s (STUArray s Vertex Vertex)
  ancestor <- newArray (1,n) 0  :: ST s (STUArray s Vertex Vertex)
  child    <- newArray (1,n) 0  :: ST s (STUArray s Vertex Vertex)
  vertex   <- newArray (1,n) 0  :: ST s (STUArray s Int    Vertex)
  label    <- newArray (0,n) 0  :: ST s (STUArray s Vertex Vertex)
  semi     <- newArray (0,n) 0  :: ST s (STUArray s Vertex Int)
  size     <- newArray (0,n) 0  :: ST s (STUArray s Vertex Int)
  pred     <- newArray (1,n) [] :: ST s (STArray  s Vertex VertexSet)
  bucket   <- newArray (1,n) [] :: ST s (STArray  s Vertex VertexSet)
  dom      <- newArray (1,n) 0
  ---------------------------------------------------------
  iRef <- newSTRef 0
  let
    dfs v = do
      modifySTRef' iRef (+ 1)
      i <- readSTRef iRef
      writeArray semi v i
      writeArray label v v
      writeArray vertex i v
      writeArray child v 0
      writeArray ancestor v 0
      writeArray size v 1
      forM_ (succ ! v) $ \w -> do
        s <- readArray semi w
        when (s == 0) $ do
          writeArray parent w v
          dfs w
        modifyArray' pred w (v:)
  ---------------------------------------------------------
    compress v = do
      a  <- readArray ancestor v
      aa <- readArray ancestor a
      when (aa /= 0) $ do
        compress a
        a' <- readArray ancestor v
        la <- readArray label a'
        sa <- readArray semi la
        lv <- readArray label v
        sv <- readArray semi lv
        when (sa < sv) $ 
          writeArray label v la
        aa' <- readArray ancestor a'
        writeArray ancestor v aa'
  ---------------------------------------------------------
    eval v = do
      a <- readArray ancestor v
      if a == 0 
        then readArray label v
        else do
          compress v
          a' <- readArray ancestor v
          la <- readArray label a'
          sa <- readArray semi la
          lv <- readArray label v
          sv <- readArray semi lv
          if sa >= sv
            then readArray label v
            else readArray label a'    
  ---------------------------------------------------------
    link v w = do
      sRef <- newSTRef w
      whileM $ do
        s  <- readSTRef sRef
        c  <- readArray child s
        dc <- readArray semi =<< readArray label c
        dw <- readArray semi =<< readArray label w
        if dw < dc
          then do
            cc  <- readArray child c
            ss  <- readArray size s
            sc  <- readArray size c
            scc <- readArray size cc
            if ss + scc >= 2 * sc
              then do
                writeArray ancestor c s
                writeArray child s cc
              else do
                writeArray size c ss
                writeArray ancestor s c
                writeSTRef sRef c
            return True
          else
            return False

      s  <- readSTRef sRef
      writeArray label s =<< readArray label w
      sw <- readArray size w
      modifyArray' size v (+ sw)
      sv <- readArray size v
      when (sv < 2 * sw) $ do
        c <- readArray child v
        writeSTRef sRef c
        writeArray child v s
      
      whileM $ do
        s' <- readSTRef sRef
        if (s' /= 0)
          then do
            writeArray ancestor s' v
            c <- readArray child s'
            writeSTRef sRef c
            return True
          else
            return False
  ---------------------------------------------------------

  -- Step 1. Number the vertices and initialize all arrays.
  dfs r  
  
  forM_ [n,n-1..2] $ \i -> do
    w <- readArray vertex i
  
    -- Step 2. Compute all semidominators.
    vs <- readArray pred w
    forM_ vs $ \v -> do
      u <- eval v
      su <- readArray semi u
      sw <- readArray semi w
      when (su < sw) $ writeArray semi w su
    sw <- readArray semi w
    vsw <- readArray vertex sw
    modifyArray' bucket vsw (w:)
    p <- readArray parent w
    link p w
  
    -- Step 3. Implicitly define the immediate dominators.
    vs2 <- readArray bucket p
    forM_ vs2 $ \v -> do
      modifyArray' bucket p (List.delete v)
      u <- eval v
      su <- readArray semi u
      sv <- readArray semi v
      writeArray dom v (if su < sv then u else p)  
  
  -- Step 4. Explicitly define the remaining immediate dominators.
  forM_ [2..n] $ \i -> do
    w <- readArray vertex i
    d <- readArray dom w
    s <- readArray vertex =<< readArray semi w
    when (d /= s) $ do
      dd <- readArray dom d
      writeArray dom w dd
  writeArray dom r 0
  
  return dom
