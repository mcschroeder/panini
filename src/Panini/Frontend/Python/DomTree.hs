{-# LANGUAGE RecordWildCards #-}

module Panini.Frontend.Python.DomTree where

import Data.Array.Unboxed
import Data.Bifunctor
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.List qualified as List
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
  live = Map.map (IntSet.map toVertex) (liveVariables cfg (Map.keys vars))
  phis = phiPlacements vars live frnt

------------------------------------------------------------------------------

variableAssignments :: Typed CFG a -> Map (String,PyType) LabelSet
variableAssignments cfg = Map.fromListWith (<>) 
  [ (v, IntSet.singleton l) | (l,n) <- IntMap.assocs cfg.nodeMap
                            , Assigned v <- Set.toList (variables f n)
  ]
 where
  f ident = (ident_string ident, typeOf ident)

-- TODO: improve performance
liveVariables :: Typed CFG a -> [(String,PyType)] -> Map (String,PyType) LabelSet
liveVariables cfg allVars = Map.fromListWith (<>) 
  [ (v, IntSet.singleton l) | l <- IntMap.keys cfg.nodeMap
                            , v <- allVars
                            , live [] v l
  ]
 where
  f ident = (ident_string ident, typeOf ident)

  live ls v l | l `elem` ls         = False
              | useBeforeAssign v n = True
              | assigned        v n = False
              | otherwise           = any (live (l:ls) v) (successors n)
   where
    n = (IntMap.!) cfg.nodeMap l
  
  assigned v = not . null . Set.filter (== Assigned v) . variables f
  
  useBeforeAssign v = \case
    Exit           -> False
    FunDef    {..} -> v `elem` (exprVarsN f $ paramDefaults _args)
    Branch    {..} -> v `elem` (exprVars f _cond)
    BranchFor {..} -> v `elem` (exprVars f _generator)
    Block     {..} -> go _stmts
     where
      go []          = False
      go (stmt:rest) = case stmtVars f stmt of
        mentions | Set.member (Used v) mentions     -> True
                 | Set.member (Assigned v) mentions -> False
                 | otherwise                        -> go rest

------------------------------------------------------------------------------

-- for debug purposes only
instance Graphviz (Typed DomTree a) where
  dot dom0 = Digraph $ go dom0 "_" dom0.root
   where
    go dom p k = case dom.nodes IntMap.! k of
      FunDef{..} -> 
        let label = "def" <+> pretty _name <> prettyTuple _args 
                    <> maybe mempty (\r -> " ->" <+> pretty r) _result 
                    <> ": ..."
        in 
          [ node dom p k label
          , conEdge p k _next ""
          , cluster _name.ident_string (domTree _body)
          ] ++ map (excEdge p k) _except
            ++ goChildren dom p k

      Block{..} -> 
        let label = mconcat $ map (<> "\\l") $ map pretty _stmts
        in
          [ node dom p k label
          , conEdge p k _next ""
          ] ++ map (excEdge p k) _except
            ++ goChildren dom p k        
    
      Branch{..} -> 
        let label = "if" <+> pretty _cond
        in
          [ node dom p k label
          , conEdge p k _nextTrue "true"
          , conEdge p k _nextFalse "false"
          ] ++ map (excEdge p k) _except
            ++ goChildren dom p k
    
      BranchFor{..} -> 
        let label = "for" <+> prettyTuple' _targets 
                    <+> "in" <+> pretty _generator
        in
          [ node dom p k label
          , conEdge p k _nextMore "more"
          , conEdge p k _nextDone "done"
          ] ++ map (excEdge p k) _except
            ++ goChildren dom p k

      Exit -> [node dom p k "exit"] ++ goChildren dom p k

    goChildren dom p k = 
      let ks = dom.children IntMap.! k
      in map (domEdge p k) ks ++ concatMap (go dom p) ks

    node d p k s = Node (p <> show k) 
      [ Shape Box, Label (phis d k <> s)
      , XLabel (pretty k), Other "nojustify" "true"
      ]
  
    conEdge p a b s = Edge (p <> show a) (p <> show b) 
      [Other "color" "blue", Label s, Other "fontcolor" "blue"]
    
    domEdge p a b = Edge (p <> show a) (p <> show b) 
      [Other "arrowhead" "none"]
    
    excEdge p a (e,b) = Edge (p <> show a) (p <> show b) 
      [Other "color" "red", Label $ pretty e]

    cluster k dom = Subgraph ("cluster_" <> k) 
      [Label (pretty k), Graphviz.Style Dashed]
      (go dom k dom.root)

    phis dom l = case dom.phiVars IntMap.! l of
      [] -> ""
      vs -> (mconcat $ List.intersperse ", " $ map (pretty . fst) vs) 
            <> " ← Φ \\l"

    prettyTuple' [x] = pretty x
    prettyTuple' xs  = prettyTuple xs
