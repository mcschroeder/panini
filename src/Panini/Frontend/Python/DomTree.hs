{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedLists #-}

module Panini.Frontend.Python.DomTree where

import Data.Array.Unboxed
import Data.Bifunctor
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Panini.Frontend.Python.CFG
import Panini.Frontend.Common.Dom
import Panini.Pretty
import Panini.Pretty.Graphviz as Graphviz
import Prelude hiding (succ,pred)

------------------------------------------------------------------------------

-- | The dominator tree summarizes the dominance relations between nodes in a
-- control-flow graph.
data DomTree = DomTree
  { domChildren :: IntMap LabelSet
  , domFrontier :: IntMap LabelSet
  , domTreeRoot :: Label
  }
  deriving stock (Show)

instance Graphviz DomTree where
  dot DomTree{..} = Digraph $ go domTreeRoot
   where
    go l = 
      let cs = IntSet.toAscList $ domChildren IntMap.! l 
          df = IntSet.toAscList $ domFrontier IntMap.! l
      in 
        [Node (show l) [Shape Circle, Label (pretty l)]]
        ++ map (\c -> Edge (show l) (show c) []) cs
        ++ map (\f -> Edge (show l) (show f) [Graphviz.Style Dashed]) df
        ++ concatMap go cs

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

  successors' = 
    IntSet.fromList . map toVertex . successors . getNode . toLabel
  
  r    = toVertex cfg.entry
  succ = listArray (1,n) (map successors' vertices)
  idom = dominators succ r
  tree = dominatorTree idom
  df   = dominanceFrontiers succ tree idom
  
  toLabelAssocs = map (bimap toLabel (IntSet.map toLabel))

  domChildren = IntMap.fromList $ toLabelAssocs $ tail $ assocs tree
  domFrontier = IntMap.fromList $ toLabelAssocs $ assocs df
  domTreeRoot = cfg.entry
