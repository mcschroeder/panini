{-# LANGUAGE RecordWildCards #-}

module Panini.Frontend.Python.DomTree where

import Data.Array.Unboxed
import Data.Bifunctor
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Panini.Frontend.Common.Dom
import Panini.Frontend.Common.SSA
import Panini.Frontend.Python.AST
import Panini.Frontend.Python.CFG
import Prelude hiding (succ)

------------------------------------------------------------------------------

data DomTree = DomTree
  { root     :: Label
  , nodes    :: IntMap Node
  , children :: IntMap [Label]
  , phiVars  :: IntMap [String]
  }
  deriving stock (Show)

domTree :: CFG -> DomTree
domTree cfg = DomTree {..}
 where
  root     = cfg.entry
  nodes    = cfg.nodeMap
  children = fromAss (map toLabel . IntSet.toAscList) $ tail $ assocs tree
  phiVars  = fromAss Set.toAscList $ assocs phis

  n         = IntMap.size nodes  
  labels    = IntMap.keys nodes
  vertices  = [1..n]
  labelMap  = IntMap.fromList $ zip labels vertices
  vertexMap = IntMap.fromList $ zip vertices labels
  toVertex  = (IntMap.!) labelMap
  toLabel   = (IntMap.!) vertexMap
  toNode    = (IntMap.!) nodes . toLabel
  fromAss f = IntMap.fromList . map (bimap toLabel f)  
  vertexSuc = IntSet.fromList . map toVertex . successors . toNode  

  succ = listArray (1,n) (map vertexSuc vertices)
  idom = dominators succ (toVertex root)
  tree = dominatorTree idom
  frnt = dominanceFrontiers succ tree idom
  vars = Map.map (IntSet.map toVertex) (variableAssignments cfg)
  phis = phiPlacements vars frnt

------------------------------------------------------------------------------

variableAssignments :: CFG -> Map String LabelSet
variableAssignments cfg = Map.fromListWith (<>) 
  [ (v, IntSet.singleton l) | (l,n) <- IntMap.assocs cfg.nodeMap
                            , Assigned v <- Set.toList (variables n)
  ]
