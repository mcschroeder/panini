{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedLists #-}
module Panini.Frontend.Python.CFG where

import Control.Monad.Trans.State.Strict
import Data.Foldable
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
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

children :: Node -> [Label]
children = \case
  FunDef{..} -> [_next]
  Block{..} -> _next : map snd _except
  Branch{..} -> _nextTrue : _nextFalse : map snd _except
  BranchFor{..} -> _nextMore : _nextDone : map snd _except
  Exit -> []

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
fromStatements stmts = removeOrphans $ compress $ cfg { entry }
 where
  ctx0 = Context 0 0 0 []
  cfg0 = CFG (IntMap.singleton 0 Exit) 1 0
  (entry, cfg) = runState (addStatements ctx0 stmts 0) cfg0

-- | Remove orphan nodes from the CFG.
removeOrphans :: CFG -> CFG
removeOrphans cfg = cfg { nodeMap = IntMap.restrictKeys cfg.nodeMap nonOrphans }
 where
  nonOrphans = IntSet.fromList 
             $ cfg.entry : (concatMap children $ IntMap.elems cfg.nodeMap)

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
                        , child <- children node
                        , child == k]

------------------------------------------------------------------------------

data Context = Context
  { break    :: Label
  , continue :: Label
  , return_  :: Label
  , except   :: [(ExceptClauseSpan, Label)]
  }

addStatements :: Context -> [StatementSpan] -> Label -> State CFG Label
addStatements ctx stmts next = foldrM (addStatement ctx) next stmts

addStatement :: Context -> StatementSpan -> Label -> State CFG Label
addStatement ctx stmt next = case stmt of

  While{..} -> do    
    nextFalse <- addStatements ctx while_else next
    cond <- reserveLabel
    let ctx' = ctx { break = nextFalse, continue = cond }
    nextTrue  <- addStatements ctx' while_body cond    
    insertNode cond $ Branch while_cond nextTrue nextFalse ctx.except

  For{..} -> do    
    nextDone <- addStatements ctx for_else next
    cond <- reserveLabel
    let ctx' = ctx { break = nextDone, continue = cond }
    nextMore <- addStatements ctx' for_body cond
    insertNode cond $ BranchFor for_targets for_generator nextMore nextDone ctx.except

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
      addNode $ Branch cond nextTrue nextFalse ctx.except

  Return{} -> addNode $ Block [stmt] ctx.return_ []

  Try{..} -> do
    finallyNext <- addStatements ctx try_finally next
    finallyBreak <- addStatements ctx try_finally ctx.break
    finallyContinue <- addStatements ctx try_finally ctx.continue
    finallyReturn <- addStatements ctx try_finally ctx.return_
    let ctxFinally = ctx { break = finallyBreak
                         , continue = finallyContinue
                         , return_ = finallyReturn 
                         }
    except <- mapM (addHandlers ctxFinally finallyNext) try_excepts
    else_ <- addStatements ctxFinally try_else finallyNext
    let ctxTry = ctxFinally { except }
    addStatements ctxTry try_body else_
   where
    addHandlers ctx' next' Handler{..} = do
      handler <- addStatements ctx' handler_suite next'
      return (handler_clause, handler)

  With{} -> undefined -- TODO
  AsyncWith{} -> undefined -- TODO

  Pass     {} -> return next
  Break    {} -> return ctx.break
  Continue {} -> return ctx.continue

  _ -> addNode $ Block [stmt] next ctx.except

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
          exceptEdge k l e = Edge (mkId k) (mkId l) [Label $ pretty e]
      in \(key,node) -> case node of
        FunDef{..} ->
          [ Node (mkId key) 
            [ Shape Box
            , Label ("def" <+> prettyFunSig _name _args _result <> ": ...")
            ]
          , Edge (mkId key) (mkId _next) []
          , Subgraph ("cluster" <> mkId key) 
              [ Label (prettyFunSig _name _args _result)
              , Graphviz.Style Dashed
              ]
              (fromCFG (mkId key <> "_") _body)
          ]
        
        Block{..} ->
          [ Node (mkId key) 
            [ Shape Box, Label $ vsep (map pretty _stmts) <> "\\l"
            , Other "nojustify" "true"
            ]
          , Edge (mkId key) (mkId _next) []
          ] ++ map (\(e,l) -> exceptEdge key l e) _except
        
        Branch{..} ->
          [ Node (mkId key) 
            [ Shape Ellipse
            , Label $ "if" <+> pretty _cond
            ]
          , Edge (mkId key) (mkId _nextTrue) [Label "true"]
          , Edge (mkId key) (mkId _nextFalse) [Label "false"]
          ] ++ map (\(e,l) -> exceptEdge key l e) _except

        BranchFor{..} ->
          [ Node (mkId key) 
              [ Shape Ellipse
              , Label $ "for" <+> prettyTuple' _targets <+> "in" <+> pretty _generator
              ]
          , Edge (mkId key) (mkId _nextMore) [Label "more"]
          , Edge (mkId key) (mkId _nextDone) [Label "done"]
          ] ++ map (\(e,l) -> exceptEdge key l e) _except
         where
          prettyTuple' [x] = pretty x
          prettyTuple' xs  = prettyTuple xs
    
        Exit -> [ Node (mkId key) [Shape Graphviz.None, Label "exit"] ]
    