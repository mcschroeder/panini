{-# LANGUAGE OverloadedLists #-}

{-|
This module contains low-level functions related to computing static single
assignment (SSA) form based on control-flow graphs.

References:

  * Cytron, Ron, Jeanne Ferrante, Barry K. Rosen, Mark N. Wegman, F. Kenneth
    Zadeck. 1991. "Efficiently Computing Static Single Assignment Form and the
    Control Dependence Graph." TOPLAS 13, no. 4 (October 1991): 451-490.
    https://doi.org/10.1145/115372.115320

-}
module Panini.Frontend.Common.SSA where

import Control.Monad.Extra
import Control.Monad.ST
import Data.Array.ST
import Data.Array.Unboxed
import Data.IntSet qualified as IntSet
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.STRef
import Panini.Frontend.Common.Dom
import Panini.Panic
import Prelude hiding (pred)

------------------------------------------------------------------------------

type Var = String
type VarSet = Set Var

-- | Compute the placements of ϕ-functions in a flowgraph.
--
-- The inputs to this function are the locations of each variable assignment and
-- the dominance frontiers for each vertex.
--
-- The output array maps each vertex of the input graph to the (possibly empty)
-- set of variables for which ϕ-functions have to be placed at the entrance of
-- that vertex.
--
-- This is an implementation of the algorithm by Cytron et al. (1991, Fig 11).
-- The runtime is effectively linear in the size of original variable
-- assignments in the input graph.
phiPlacements 
  :: Map Var VertexSet       -- ^ locations of variable assignments
  -> Array Vertex VertexSet  -- ^ dominance frontiers
  -> Array Vertex VarSet     -- ^ placements of ϕ-functions
phiPlacements a df = runSTArray $ do
  let (m,n) = bounds df
  assertM (m == 1)

  iterCount  <- newSTRef 0
  hasAlready <- newArray (1,n) 0 :: ST s (STUArray s Vertex Int)
  work       <- newArray (1,n) 0 :: ST s (STUArray s Vertex Int)
  w          <- newSTRef []
  phiFuncs   <- newArray (1,n) []

  forM_ (Map.keys a) $ \v -> do
    modifySTRef' iterCount (+ 1)
    i <- readSTRef iterCount
    forM_ (IntSet.toList $ a Map.! v) $ \x -> do
      writeArray work x i
      modifySTRef' w (IntSet.insert x)
    whileM $ do
      done <- IntSet.null <$> readSTRef w
      if done 
        then return False
        else do
          (x,w') <- IntSet.deleteFindMin <$> readSTRef w
          writeSTRef w w'
          forM_ (IntSet.toList $ df ! x) $ \y -> do
            hasAlreadyY <- readArray hasAlready y
            when (hasAlreadyY < i) $ do
              modifyArray' phiFuncs y (Set.insert v)
              writeArray hasAlready y i
              workY <- readArray work y
              when (workY < i) $ do
                writeArray work y i
                modifySTRef' w (IntSet.insert y)
          return True

  return phiFuncs

