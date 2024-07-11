{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedLists #-}
module Panini.Frontend.Python.SSA where

import Control.Monad.Extra
import Control.Monad.ST
import Data.Foldable
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.STRef
import Panini.Frontend.Python.AST
import Panini.Frontend.Python.CFG
import Panini.Frontend.Python.Dom
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

data Phi = Phi Var [Label]
  deriving stock (Show)

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

-- based on Cytron et al. (1991, Fig. 11)
phiPlacements :: DomTree -> CFG -> IntMap [Phi]
phiPlacements DomTree{..} cfg = runST $ do
  iterCountRef  <- newSTRef 0
  hasAlreadyRef <- newSTRef $ IntMap.map (const 0) cfg.nodeMap
  workRef       <- newSTRef $ IntMap.map (const 0) cfg.nodeMap  
  wRef          <- newSTRef mempty
  phiMap        <- newSTRef mempty

  let assignments = variableAssignments cfg
  let predecessors = cfgPredecessors cfg

  forM_ (Map.keys assignments) $ \v -> do
    modifySTRef' iterCountRef (+ 1)
    iterCount <- readSTRef iterCountRef
    forM_ (IntSet.toList $ assignments Map.! v) $ \x -> do
      modifySTRef' workRef $ IntMap.insert x iterCount
      modifySTRef' wRef $ IntSet.insert x
    whileM $ do
      w <- readSTRef wRef
      if IntSet.null w
        then return False
        else do
          x <- takeFromIntSetRef wRef
          forM_ (IntSet.toAscList $ domFrontier IntMap.! x) $ \y -> do
            hasAlready <- readSTRef hasAlreadyRef
            when (hasAlready IntMap.! y < iterCount) $ do
              let phi = Phi v (predecessors IntMap.! y)
              modifySTRef' phiMap $ IntMap.insertWith (++) y [phi]
              modifySTRef' hasAlreadyRef $ IntMap.insert y iterCount
              work <- readSTRef workRef
              when (work IntMap.! y < iterCount) $ do
                modifySTRef' workRef $ IntMap.insert y iterCount
                modifySTRef' wRef $ IntSet.insert y
          return True
  
  readSTRef phiMap

takeFromIntSetRef :: STRef s IntSet -> ST s Int
takeFromIntSetRef ref = do
  w <- readSTRef ref
  let (x,w') = IntSet.deleteFindMin w
  writeSTRef ref w'
  return x
