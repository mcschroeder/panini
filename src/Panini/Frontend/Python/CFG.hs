{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedLists #-}
module Panini.Frontend.Python.CFG
  ( Label
  , LabelSet
  , CFG(..)
  , Node(..)
  , successors
  , variables
  , fromModule
  , Error(..)
  ) where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Strict
import Data.Foldable
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Set (Set)
import Data.Set qualified as Set
import Language.Python.Common.PrettyAST ()
import Panini.Frontend.Python.AST hiding (Set)
import Panini.Frontend.Python.Pretty ()
import Panini.Pretty
import Panini.Pretty.Graphviz as Graphviz
import Prelude

------------------------------------------------------------------------------

type Label = Int
type LabelSet = IntSet

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
      , _except :: [(ExceptClauseSpan,Label)]
      }
  | Block
      { _stmts  :: [StatementSpan]
      , _next   :: Label 
      , _except :: [(ExceptClauseSpan,Label)]
      }
  | Branch
      { _cond      :: ExprSpan
      , _nextTrue  :: Label
      , _nextFalse :: Label
      , _except    :: [(ExceptClauseSpan,Label)]
      }
  | BranchFor
      { _targets   :: [ExprSpan]
      , _generator :: ExprSpan
      , _nextMore  :: Label
      , _nextDone  :: Label
      , _except    :: [(ExceptClauseSpan,Label)]
      }
  | Exit
  deriving stock (Show)

successors :: Node -> [Label]
successors = \case
  FunDef    {..} -> [_next]
  Block     {..} -> _next : map snd _except
  Branch    {..} -> _nextTrue : _nextFalse : map snd _except
  BranchFor {..} -> _nextMore : _nextDone : map snd _except
  Exit           -> []

variables :: Node -> Set VarMention
variables = Set.unions . \case
  FunDef    {}   -> []
  Block     {..} -> map stmtVars _stmts
  Branch    {..} -> [used _cond]
  BranchFor {..} -> [assdN _targets] ++ [used _generator]
  Exit           -> []
 where
  used  = Set.map Used . exprVars
  assdN = Set.map Assigned . exprVarsN

------------------------------------------------------------------------------

data Error = Unsupported StatementSpan
  deriving stock (Show)

fromModule :: ModuleSpan -> Either Error CFG
fromModule (Module stmts) = fromStatements stmts

fromStatements :: [StatementSpan] -> Either Error CFG
fromStatements stmts = 
  fixup <$> runExcept (runStateT (addStatements ctx0 stmts 0) cfg0)
 where  
  ctx0 = Context 0 0 0 []
  cfg0 = CFG (IntMap.singleton 0 Exit) 1 0
  fixup (entry, cfg) = removeOrphans $ compress $ cfg { entry }

-- | Remove orphan nodes from the CFG.
removeOrphans :: CFG -> CFG
removeOrphans cfg = cfg { nodeMap = IntMap.restrictKeys cfg.nodeMap nonOrphans }
 where
  nonOrphans = IntSet.fromList 
             $ cfg.entry : (concatMap successors $ IntMap.elems cfg.nodeMap)

-- | Merge consecutive CFG blocks, if it is safe to do so.
compress :: CFG -> CFG
compress cfg = cfg { nodeMap = foldl' go cfg.nodeMap keys0 }
 where
  keys0 = IntMap.keys cfg.nodeMap

  go m k | Just b1@Block{} <- IntMap.lookup k m
         , Just b2@Block{} <- IntMap.lookup b1._next m
         , [k'] <- parents b1._next m, k' == k
         , b1._except == b2._except
         , let b1' = b1 { _stmts = b1._stmts ++ b2._stmts, _next = b2._next }
         = IntMap.insert k b1' $ IntMap.delete b1._next m
  go m _ = m

  parents k m = [parent | (parent, node) <- IntMap.assocs m
                        , child <- successors node
                        , child == k]

------------------------------------------------------------------------------

type CFGBuilder a = StateT CFG (Except Error) a

addNode :: Node -> CFGBuilder Label
addNode n = do
  l <- reserveLabel
  insertNode l n

reserveLabel :: CFGBuilder Label
reserveLabel = do
  l <- gets nextLabel
  modify' $ \g -> g { nextLabel = l + 1 }
  return l

insertNode :: Label -> Node -> CFGBuilder Label
insertNode l n = do
  modify' $ \g -> g { nodeMap = IntMap.insert l n g.nodeMap }
  return l

------------------------------------------------------------------------------

data Context = Context
  { break    :: Label
  , continue :: Label
  , return_  :: Label
  , excepts   :: [(ExceptClauseSpan, Label)]
  }

addStatements :: Context -> [StatementSpan] -> Label -> CFGBuilder Label
addStatements ctx stmts next = foldrM (addStatement ctx) next stmts

addStatement :: Context -> StatementSpan -> Label -> CFGBuilder Label
addStatement ctx stmt next = case stmt of

  While{..} -> do    
    nextFalse <- addStatements ctx while_else next
    cond <- reserveLabel
    let ctx' = ctx { break = nextFalse, continue = cond }
    nextTrue  <- addStatements ctx' while_body cond    
    insertNode cond $ Branch 
      { _cond      = while_cond
      , _nextTrue  = nextTrue
      , _nextFalse = nextFalse
      , _except    = ctx.excepts
      }

  For{..} -> do    
    nextDone <- addStatements ctx for_else next
    cond <- reserveLabel
    let ctx' = ctx { break = nextDone, continue = cond }
    nextMore <- addStatements ctx' for_body cond
    insertNode cond $ BranchFor 
      { _targets    = for_targets
      , _generator  = for_generator
      , _nextMore   = nextMore
      , _nextDone   = nextDone
      , _except     = ctx.excepts
      }

  Fun{..} -> do
    body <- lift $ except $ fromStatements (stripDocstring fun_body)
    addNode $ FunDef 
      { _name   = fun_name
      , _args   = fun_args
      , _result = fun_result_annotation
      , _body   = body
      , _next   = next
      , _except = ctx.excepts
      }
   where
    stripDocstring (StmtExpr{stmt_expr = Strings{}} : xs) = xs
    stripDocstring                                    xs  = xs

  Conditional{..} -> do
    else_ <- addStatements ctx cond_else next
    foldrM addGuard else_ cond_guards
   where
    addGuard (cond,body) nextFalse = do
      nextTrue <- addStatements ctx body next
      addNode $ Branch 
        { _cond      = cond
        , _nextTrue  = nextTrue
        , _nextFalse = nextFalse
        , _except    = ctx.excepts
        }

  Return{} -> addNode $ Block 
    { _stmts  = [stmt]
    , _next   = ctx.return_
    , _except = ctx.excepts
    }

  Try{..} -> do
    finallyNext <- addStatements ctx try_finally next
    finallyBreak <- addStatements ctx try_finally ctx.break
    finallyContinue <- addStatements ctx try_finally ctx.continue
    finallyReturn <- addStatements ctx try_finally ctx.return_
    let ctxFinally = ctx { break = finallyBreak
                         , continue = finallyContinue
                         , return_ = finallyReturn 
                         }
    excepts <- mapM (addHandlers ctxFinally finallyNext) try_excepts
    else_ <- addStatements ctxFinally try_else finallyNext
    let ctxTry = ctxFinally { excepts }
    addStatements ctxTry try_body else_
   where
    addHandlers ctx' next' Handler{..} = do
      handler <- addStatements ctx' handler_suite next'
      return (handler_clause, handler)

  Pass     {} -> return next
  Break    {} -> return ctx.break
  Continue {} -> return ctx.continue

  AugmentedAssign{..} -> do
    let expr' = BinaryOp { operator     = assignOpToOp aug_assign_op
                         , left_op_arg  = aug_assign_to
                         , right_op_arg = aug_assign_expr
                         , expr_annot   = stmt_annot
                         }
    let stmt' = Assign { assign_to   = [aug_assign_to]
                       , assign_expr = expr'
                       , stmt_annot
                       }
    addNode $ Block { _stmts  = [stmt']
                    , _next   = next
                    , _except = ctx.excepts
                    }

  AsyncFor  {} -> lift $ throwE $ Unsupported stmt
  AsyncFun  {} -> lift $ throwE $ Unsupported stmt
  Class     {} -> lift $ throwE $ Unsupported stmt
  Decorated {} -> lift $ throwE $ Unsupported stmt
  With      {} -> lift $ throwE $ Unsupported stmt   -- TODO: support with
  AsyncWith {} -> lift $ throwE $ Unsupported stmt
  Global    {} -> lift $ throwE $ Unsupported stmt
  NonLocal  {} -> lift $ throwE $ Unsupported stmt

  _ -> addNode $ Block 
    { _stmts  = [stmt]
    , _next   = next
    , _except = ctx.excepts
    }

------------------------------------------------------------------------------

instance Pretty CFG where
  pretty CFG{..} = prettyMap $ map markEntry $ IntMap.toDescList $ nodeMap
   where
    markEntry (k,n) | k == entry = (arrow <> pretty k, pretty n)
                    | otherwise  = (pretty k, pretty n)

instance Pretty Node where
  pretty = \case
    FunDef{..} -> nest 2 $ 
      "def" <+> prettyFunSig _name _args _result 
      <+> "then" <+> arrow <> pretty _next 
      <\> pretty _body    
    Block{..} -> 
      pretty _stmts <+> "then" <+> arrow <> pretty _next <+> prettyMap _except
    Branch{..} -> 
      "if" <+> pretty _cond <+> "then" <+> arrow <> pretty _nextTrue 
      <+> "else" <+> arrow <> pretty _nextFalse <+> prettyMap _except    
    BranchFor{..} ->
      "for" <+> pretty _targets <+> "in" <+> pretty _generator 
      <+> "do" <+> arrow <> pretty _nextMore 
      <+> "else" <+> arrow <> pretty _nextDone 
      <+> prettyMap _except    
    Exit -> "exit"

prettyFunSig :: IdentSpan -> [ParameterSpan] -> Maybe ExprSpan -> Doc
prettyFunSig name args res = 
  pretty name <> prettyTuple args <> maybe mempty (\r -> " ->" <+> pretty r) res

instance Graphviz CFG where
  dot = Digraph . fromCFG "_"
   where
    fromCFG :: String -> CFG -> [Graphviz.Statement]
    fromCFG prefix cfg = 
      concatMap (fromNode prefix) (IntMap.toList cfg.nodeMap)
      ++ [ Node (prefix <> "entry") [Shape Graphviz.None, Label "entry"]
         , Edge (prefix <> "entry") (prefix <> show cfg.entry) [] 
         ]

    fromNode :: String -> (Label, Node) -> [Graphviz.Statement]
    fromNode prefix =
      let mkId k = prefix <> (show k)
          exceptEdge k (e,l) = Edge (mkId k) (mkId l) [Label $ pretty e]
          boxNode k lbl = 
            Node (mkId k) 
              [ Shape Box
              , Label lbl
              , Other "nojustify" "true"
              , XLabel (pretty k)
              ]
      in \(key,node) -> case node of
        FunDef{..} ->
          [ boxNode key ("def" <+> prettyFunSig _name _args _result <> ": ...")
          , Edge (mkId key) (mkId _next) []
          , Subgraph ("cluster" <> mkId key) 
              [ Label (prettyFunSig _name _args _result)
              , Graphviz.Style Dashed
              ]
              (fromCFG (mkId key <> "_") _body)
          ]
        
        Block{..} ->
          [ boxNode key (mconcat $ map (<> "\\l") $ map pretty _stmts)
          , Edge (mkId key) (mkId _next) []
          ] ++ map (exceptEdge key) _except
        
        Branch{..} ->
          [ boxNode key ("if" <+> pretty _cond)
          , Edge (mkId key) (mkId _nextTrue) [Label "true"]
          , Edge (mkId key) (mkId _nextFalse) [Label "false"]
          ] ++ map (exceptEdge key) _except

        BranchFor{..} ->
          [ boxNode key ("for" <+> prettyTuple' _targets <+> "in" <+> pretty _generator)
          , Edge (mkId key) (mkId _nextMore) [Label "more"]
          , Edge (mkId key) (mkId _nextDone) [Label "done"]
          ] ++ map (exceptEdge key) _except
         where
          prettyTuple' [x] = pretty x
          prettyTuple' xs  = prettyTuple xs
    
        Exit -> [ Node (mkId key) [Shape Graphviz.None, Label "exit"] ]
    