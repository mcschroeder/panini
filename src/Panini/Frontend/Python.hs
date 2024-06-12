{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedLists #-}
module Panini.Frontend.Python (loadPythonSource) where

import Language.Python.Version3
import Language.Python.Common.Pretty qualified as Py
import Language.Python.Common.PrettyAST ()
import Panini.Error
import Panini.Monad
import Panini.Provenance
import Prelude
import System.Directory
import Panini.Pretty
import System.FilePath (normalise)
import Language.Python.Common.AST
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.IntMap.Strict (IntMap)
import Data.IntSet (IntSet)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Text.PrettyPrint qualified as TPP

loadPythonSource :: FilePath -> Pan ()
loadPythonSource fp = do
  src <- tryIO NoPV $ readFile fp
  case parseModule src fp of
    Left err -> undefined --throwError $ ParserError undefined undefined
    Right (mod,cmts) -> do
      logData $ pretty $ Py.prettyText mod
      let cfg = moduleToCFG mod
      logData $ pretty cfg


moduleToCFG :: ModuleSpan -> CFG
moduleToCFG (Module [Fun{..}]) = g3
 where
  g0 = MkCFG { nodeMap = IntMap.empty, nextKey = 0 }
  (exitLabel, g1) = addNode Exit g0  
  (bodyLabel, g2) = build fun_body exitLabel g1
  entryNode = Entry fun_name fun_args fun_result_annotation bodyLabel
  (_, g3) = addNode entryNode g2

moduleToCFG _ = undefined


build :: [StatementSpan] -> Label -> CFG -> (Label, CFG)
build (Conditional{cond_guards=[(cond,body)],..} : stmts) exitLabel g0 = addNode ifNode g3
 where  
  (nextLabel, g1) = build stmts exitLabel g0
  (bodyLabel, g2) = build body nextLabel g1
  (elseLabel, g3) = build cond_else nextLabel g2
  ifNode = Branch cond bodyLabel elseLabel

build [] exitLabel g = (exitLabel, g)

build stmts exitLabel g0 = addNode (Block stmts exitLabel) g0


type Label = Int
data CFG = MkCFG { nodeMap :: IntMap Node, nextKey :: Int }
  deriving stock (Show)

data Node 
  = Entry { _name :: IdentSpan, _args :: [ParameterSpan], _result :: Maybe ExprSpan, _exit :: Label }
  | Block { _stmts :: [StatementSpan], _exit :: Label }
  | Branch { _cond :: ExprSpan, _exitTrue :: Label, _exitFalse :: Label }
  | Exit
  deriving stock (Show)


addNode :: Node -> CFG -> (Label, CFG)
addNode n g = (l,g')
 where
  l = g.nextKey
  g' = MkCFG { nodeMap = IntMap.insert l n g.nodeMap, nextKey = l + 1 }

instance Pretty CFG where
  pretty g = pretty g.nodeMap

instance Pretty Node where
  pretty = \case
    Entry{..} -> "def" <+> pretty _name <> prettyTuple _args <> colon <+> arrow <+> pretty _exit
    Block{..} -> pretty _stmts <+> arrow <+> pretty _exit
    Branch{..} -> "if" <+> pretty _cond <+> arrow <+> pretty _exitTrue <+> "or" <+> pretty _exitFalse
    Exit -> "Exit"

instance Pretty IdentSpan where
  pretty = pretty . TPP.render . Py.pretty

instance Pretty ParameterSpan where
  pretty = pretty . TPP.render . Py.pretty

instance Pretty StatementSpan where
  pretty = pretty . TPP.render . Py.pretty

instance Pretty ExprSpan where
  pretty = pretty . TPP.render . Py.pretty
