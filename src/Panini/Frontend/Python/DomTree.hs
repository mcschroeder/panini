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
import Panini.Frontend.Python.Typing.PyType (PyType)
import Panini.Frontend.Python.Typing.TypeInfo
import Panini.Pretty
import Panini.Pretty.Graphviz as Graphviz
import Prelude hiding (succ)

------------------------------------------------------------------------------

data DomTree a = DomTree
  { root     :: Label
  , nodes    :: IntMap (Node a)
  , children :: IntMap [Label]
  , phiVars  :: IntMap [(String, PyType)]
  }
  deriving stock (Show)

domTree :: Typed CFG a -> Typed DomTree a
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

variableAssignments :: Typed CFG a -> Map (String,PyType) LabelSet
variableAssignments cfg = Map.fromListWith (<>) 
  [ (v, IntSet.singleton l) | (l,n) <- IntMap.assocs cfg.nodeMap
                            , Assigned v <- Set.toList (variables f n)
  ]
 where
  f ident = (ident_string ident, typeOf ident)

------------------------------------------------------------------------------

-- for debug purposes only
instance Graphviz (Typed DomTree a) where
  dot = Digraph . go0 "_"
   where
    go0 prefix dom = go prefix dom dom.root
    
    go prefix dom l = case dom.nodes IntMap.! l of
      FunDef{..} -> go2 prefix dom l ++
        [ Subgraph ("cluster_" <> _name.ident_string) 
            [ Label (pretty $ _name.ident_string)
            , Graphviz.Style Dashed
            ] 
            (go0 _name.ident_string (domTree _body))
        ]
      _ -> go2 prefix dom l

    go2 prefix dom l = 
      let mkKey k = prefix <> show k in
      [ Node (mkKey l) [Shape Circle, Label (pretty $ show l) ] ] 
      ++ map (\r -> Edge (mkKey l) (mkKey r) []) (dom.children IntMap.! l)
      ++ concatMap (go prefix dom) (dom.children IntMap.! l)
