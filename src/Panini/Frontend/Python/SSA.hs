{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedLists #-}
module Panini.Frontend.Python.SSA where

import Data.Array
import Data.Bifunctor
import Data.Foldable
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Panini.Frontend.Common.SSA
import Panini.Frontend.Python.AST hiding (Var)
import Panini.Frontend.Python.AST qualified as Py
import Panini.Frontend.Python.CFG
import Panini.Frontend.Python.DomTree
import Language.Python.Common.SrcLocation
import Prelude

------------------------------------------------------------------------------

variableAssignments :: CFG -> Map Var LabelSet
variableAssignments cfg = 
  Map.fromListWith (<>) 
    [ (v,[l]) 
    | (l,n) <- IntMap.assocs cfg.nodeMap
    , Assigned v <- toList (nodeVars n) 
    ]

nodeVars :: Node -> Set VarMention
nodeVars = Set.unions . \case
  FunDef    {}   -> []
  Block     {..} -> map stmtVars _stmts
  Branch    {..} -> [used _cond]
  BranchFor {..} -> [assdN _targets] ++ [used _generator]
  Exit           -> []
 where
  used  = Set.map Used . exprVars
  assdN = Set.map Assigned . exprVarsN

cfgPredecessors :: CFG -> IntMap [Label]
cfgPredecessors = foldl' go mempty . IntMap.assocs . nodeMap
 where
  go m (l,n) = 
    let addPredFor = 
          IntMap.unionWith (++) m . IntMap.fromList . map (\x -> (x,[l]))
    in case n of
      FunDef    {..} -> addPredFor [_next]
      Block     {..} -> addPredFor [_next]
      Branch    {..} -> addPredFor $ [_nextTrue,_nextFalse] ++ map snd _except
      BranchFor {..} -> addPredFor $ [_nextMore,_nextDone] ++ map snd _except
      Exit           -> m

phiFuncs :: DomTree -> CFG -> IntMap VarSet
phiFuncs dt cfg = phi'
 where
  n         = IntMap.size cfg.nodeMap
  labels    = IntMap.keys cfg.nodeMap
  vertices  = [1..n]
  labelMap  = IntMap.fromList $ zip labels vertices
  vertexMap = IntMap.fromList $ zip vertices labels
  toVertex  = (IntMap.!) labelMap
  toLabel   = (IntMap.!) vertexMap

  toVertexAssocs = map (bimap toVertex (IntSet.map toVertex))

  a   = Map.map (IntSet.map toVertex) $ variableAssignments cfg
  df  = array (1,n) $ toVertexAssocs $ IntMap.toList dt.domFrontier
  phi = phiPlacements a df

  phi' = IntMap.fromList $ map (first toLabel) $ assocs phi


placePhiFunctions :: IntMap VarSet -> CFG -> CFG
placePhiFunctions phis cfg = cfg { nodeMap = IntMap.mapWithKey go cfg.nodeMap}
 where
  numPreds = IntMap.map length $ cfgPredecessors cfg

  go _ Exit = Exit
  go l n = case phis IntMap.! l of
    [] -> n
    vs -> n { _stmts = map (mkPhi l) (Set.toAscList vs) ++ n._stmts}
  
  mkPhi l v = PhiAssign v (replicate (numPreds IntMap.! l) v)

