{-|

References:

  * Lengauer, Thomas and Robert Endre Tarjan. 1979. "A Fast Algorithm for
    Finding Dominators in a Flowgraph." TOPLAS 1, no. 1 (July 1979): 121-141.
    https://doi.org/10.1145/357062.357071

-}
module Panini.Frontend.Python.Dom where

import Control.Monad.ST
import Panini.Frontend.Python.CFG
import Prelude hiding (succ,pred)
import Data.IntMap.Strict qualified as IntMap
import Data.STRef
import Control.Monad.Extra
import Data.Array.Unboxed
import Data.Array.ST
import Data.List qualified as List
import Data.Maybe
import Data.Bifunctor

-- TODO
idom :: CFG -> [(Label, Maybe Label)]
idom cfg = idomCfg
 where
  dom2cfgMap = IntMap.fromList $ zip [1..] (IntMap.keys cfg.nodeMap)  
  cfg2domMap = IntMap.fromList $ zip (IntMap.keys cfg.nodeMap) [1..]

  dom2cfg  v = IntMap.lookup v dom2cfgMap
  dom2cfg' v = fromJust $ dom2cfg v
  
  cfg2dom  v = IntMap.lookup v cfg2domMap
  cfg2dom' v = fromJust $ cfg2dom v

  cfgNode l = fromJust $ IntMap.lookup l cfg.nodeMap
  
  r = cfg2dom' cfg.entry
  n = IntMap.size cfg.nodeMap

  succ = listArray (1,n) 
       $ map (map cfg2dom' . children . cfgNode) 
       $ IntMap.elems dom2cfgMap

  idoms = dominators succ r n

  idomCfg = map (bimap dom2cfg' dom2cfg) $ assocs idoms

------------------------------------------------------------------------------

type Vertex = Int

-- | Compute all immediate dominators in a flowgraph.
--
-- The input graph is given as an array of sets of (control-flow) successor
-- vertices, together with an initial vertex and the total number of vertices.
-- Note that the set of vertices V = { v | 1 ≤ v ≤ n } is expected to be a
-- contiguous range of integers, starting at 1.
--
-- This is an implementation of the "sophisticated" version of the
-- Lengauer-Tarjan algorithm (1979) and closely follows their exposition in the
-- paper. The runtime is O(m*α(m,n)), where m and n are the numbers of edges and
-- vertices in the input graph, and α(m,n) is a functional inverse of Ackerman's
-- function.
dominators :: Array Vertex [Vertex] -> Vertex -> Int -> UArray Vertex Vertex
dominators succ r n = runSTUArray $ do
  parent   <- newArray (1,n) 0  :: ST s (STUArray s Vertex Vertex)
  ancestor <- newArray (1,n) 0  :: ST s (STUArray s Vertex Vertex)
  child    <- newArray (1,n) 0  :: ST s (STUArray s Vertex Vertex)
  vertex   <- newArray (1,n) 0  :: ST s (STUArray s Int    Vertex)
  label    <- newArray (0,n) 0  :: ST s (STUArray s Vertex Vertex)
  semi     <- newArray (0,n) 0  :: ST s (STUArray s Vertex Int)
  size     <- newArray (0,n) 0  :: ST s (STUArray s Vertex Int)
  pred     <- newArray (1,n) [] :: ST s (STArray  s Vertex [Vertex])  
  bucket   <- newArray (1,n) [] :: ST s (STArray  s Vertex [Vertex])  
  dom      <- newArray (1,n) 0
  ----------------------------------------------------------------------------
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
  ----------------------------------------------------------------------------
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
  ----------------------------------------------------------------------------
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
  ----------------------------------------------------------------------------
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
  ----------------------------------------------------------------------------

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
