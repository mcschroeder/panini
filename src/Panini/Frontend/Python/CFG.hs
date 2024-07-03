{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedLists #-}
module Panini.Frontend.Python.CFG where

import Control.Monad.Trans.State.Strict
import Data.Foldable
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Language.Python.Common.AST as Py
import Language.Python.Common.PrettyAST ()
import Panini.Frontend.Python.Pretty ()
import Panini.Pretty
import Panini.Pretty.Graphviz as Graphviz
import Prelude

------------------------------------------------------------------------------

type Label = Int

data CFG = CFG 
  { nodeMap   :: IntMap Node
  , nextLabel :: Label
  , entry     :: Label
  }
  deriving stock (Show)

data Node
  = FunDef
      { _name   :: IdentSpan
      , _args   :: [ParameterSpan]
      , _result :: Maybe ExprSpan
      , _body   :: CFG
      , _next   :: Label
      }
  | Block
      { _stmts :: [StatementSpan]
      , _next  :: Label 
      }
  | Branch
      { _cond      :: ExprSpan
      , _nextTrue  :: Label
      , _nextFalse :: Label
      }
  | BranchFor
      { _targets   :: [ExprSpan]
      , _generator :: ExprSpan
      , _nextMore  :: Label
      , _nextDone  :: Label
      }
  | Exit
  deriving stock (Show)

------------------------------------------------------------------------------

instance Pretty CFG where
  pretty CFG{..} = "entry" <+> arrow <> pretty entry <\> pretty nodeMap

instance Pretty Node where
  pretty = \case
    FunDef  {..} -> nest 2 $ 
      "def" <+> prettyFunSig _name _args _result <>
      "then" <+> arrow <> pretty _next 
      <\> pretty _body
    
    Block   {..} -> pretty _stmts <+> "then" <+> arrow <> pretty _next
    
    Branch  {..} -> 
      "if" <+> pretty _cond <+> "then" <+> arrow <> pretty _nextTrue <+>
      "else" <+> arrow <> pretty _nextFalse
    
    BranchFor {..} -> 
      "for" <+> pretty _targets <+> "in" <+> pretty _generator <+> 
      "do" <+> arrow <> pretty _nextMore <+> 
      "else" <+> arrow <> pretty _nextDone
    
    Exit -> "exit"

prettyFunSig :: IdentSpan -> [ParameterSpan] -> Maybe ExprSpan -> Doc
prettyFunSig name args res = 
  pretty name <> prettyTuple args <> maybe mempty (\r -> " ->" <+> pretty r) res

instance Graphviz CFG where
  dot = Digraph . fromCFG "_"
   where
    fromCFG :: String -> CFG -> [Graphviz.Statement]
    fromCFG prefix = concatMap (fromNode prefix) . IntMap.toList . nodeMap

    fromNode :: String -> (Label, Node) -> [Graphviz.Statement]
    fromNode prefix =
      let mkId k = prefix <> (show k) 
      in \(key,node) -> case node of
        FunDef{..} ->          
          [ Node (mkId key) 
            [ Shape Box
            , Label ("def" <+> prettyFunSig _name _args _result <> ": ...")
            ]
          , Edge (mkId key) (mkId _next) []
          , Subgraph ("cluster" <> mkId key) 
              [ Label (prettyFunSig _name _args _result)
              , Graphviz.Style Rounded
              ]
              (fromCFG (mkId key <> "_") _body)
          ]
        
        Block{..} ->
          [ Node (mkId key) [Shape Box, Label $ vsep $ map pretty _stmts]
          , Edge (mkId key) (mkId _next) []
          ]
        
        Branch{..} ->
          [ Node (mkId key) [Shape Diamond, Label $ pretty _cond]
          , Edge (mkId key) (mkId _nextTrue) [Label "true"]
          , Edge (mkId key) (mkId _nextFalse) [Label "false"]
          ]

        BranchFor{..} ->
          [ Node (mkId key) 
              [ Shape Diamond
              , Label $ pretty _targets <+> "in" <+> pretty _generator
              ]
          , Edge (mkId key) (mkId _nextMore) [Label "more"]
          , Edge (mkId key) (mkId _nextDone) [Label "done"]
          ]
    
        Exit -> [ Node (mkId key) [Shape Graphviz.None, Label "exit"] ]

------------------------------------------------------------------------------

addNode :: Node -> State CFG Label
addNode n = do
  l <- reserveLabel
  insertNode l n

reserveLabel :: State CFG Label
reserveLabel = do
  l <- gets nextLabel
  modify' $ \g -> g { nextLabel = l + 1 }
  return l

insertNode :: Label -> Node -> State CFG Label
insertNode l n = do
  modify' $ \g -> g { nodeMap = IntMap.insert l n g.nodeMap }
  return l

------------------------------------------------------------------------------

fromModule :: ModuleSpan -> CFG
fromModule (Module stmts) = fromStatements stmts

fromStatements :: [StatementSpan] -> CFG
fromStatements stmts = cfg { entry }
 where
  ctx0 = Context undefined undefined
  cfg0 = CFG (IntMap.singleton 0 Exit) 1 0
  (entry, cfg) = runState (addStatements ctx0 stmts 0) cfg0

data Context = Context
  { break    :: Label
  , continue :: Label
  }

addStatements :: Context -> [StatementSpan] -> Label -> State CFG Label
addStatements ctx stmts next = foldrM (addStatement ctx) next stmts

addStatement :: Context -> StatementSpan -> Label -> State CFG Label
addStatement ctx stmt next = case stmt of

  While{..} -> do    
    nextFalse <- addStatements ctx while_else next
    cond <- reserveLabel
    let ctx' = Context { break = nextFalse, continue = cond }
    nextTrue  <- addStatements ctx' while_body cond
    insertNode cond $ Branch while_cond nextTrue nextFalse

  For{..} -> do    
    nextDone <- addStatements ctx for_else next
    cond <- reserveLabel
    let ctx' = Context { break = nextDone, continue = cond }
    nextMore <- addStatements ctx' for_body cond
    insertNode cond $ BranchFor for_targets for_generator nextMore nextDone

  AsyncFor{} -> undefined -- TODO

  Fun{..} -> do
    let body = fromStatements (stripDocstring fun_body)
    addNode $ FunDef fun_name fun_args fun_result_annotation body next
   where
    stripDocstring (StmtExpr{stmt_expr = Strings{}} : xs) = xs
    stripDocstring                                    xs  = xs

  AsyncFun{} -> undefined -- TODO

  Class{} -> undefined -- TODO

  Conditional{..} -> do
    else_ <- addStatements ctx cond_else next
    foldrM addGuard else_ cond_guards
   where
    addGuard (cond,body) nextFalse = do
      nextTrue <- addStatements ctx body next
      addNode $ Branch cond nextTrue nextFalse

  Return{} -> addNode $ Block [stmt] 0

  Try{} -> undefined -- TODO
  Raise{} -> undefined -- TODO

  With{} -> undefined -- TODO
  AsyncWith{} -> undefined -- TODO

  Pass     {} -> return next
  Break    {} -> return ctx.break
  Continue {} -> return ctx.continue

  _ -> do
    nextNode <- IntMap.lookup next <$> gets nodeMap
    case nextNode of
      Just (Block xs next2) -> insertNode next (Block (stmt:xs) next2)
      _ -> addNode $ Block [stmt] next
