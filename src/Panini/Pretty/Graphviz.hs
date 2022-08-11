{-# LANGUAGE OverloadedStrings #-}

module Panini.Pretty.Graphviz where

import Control.Monad
import Control.Monad.Trans.State.Strict
import Data.List qualified as List
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Lazy qualified as LT
import Data.Text.Lazy.Builder (Builder)
import Data.Text.Lazy.Builder qualified as LB
import Prelude
import System.IO.Error
import System.IO.Unsafe
import System.Process

-------------------------------------------------------------------------------

class GraphViz a where  
  dot :: a -> Dot ()

renderDOT :: GraphViz a => a -> Text
renderDOT x =
  let Dot d = dot x
      stmts = snd $ execState d (0, mempty)
      graph = "digraph {\n" <> stmts <> "}\n"
  in LT.toStrict $ LB.toLazyText graph

renderGraph :: GraphViz a => FilePath -> a -> IO ()
renderGraph f x = do  
  let d' = Text.unpack $ renderDOT x
  let args = ["-Tpng", "-o" ++ f]
  void $ readProcess "dot" args d'

traceGraph :: GraphViz a => FilePath -> a -> a
traceGraph f x = unsafePerformIO $ do
  putStrLn $ "rendering graph to " ++ f
  catchIOError (renderGraph f x) print
  return x
  
-------------------------------------------------------------------------------

newtype Dot a = Dot (State (NodeId, Builder) a)
  deriving newtype (Functor, Applicative, Monad)

type NodeId = Int

data Attribute
  = Shape Shape
  | Label Text

data Shape = Circle | Box

mkNode :: [Attribute] -> Dot NodeId
mkNode as = Dot $ do
  (n,b) <- get
  let n' = n + 1
  let b' = b <> nodeStmt n' as
  put (n',b')
  return n'

mkEdge :: NodeId -> NodeId -> Dot ()
mkEdge x y = Dot $ do
  (n,b) <- get
  let b' = b <> edgeStmt x y
  put (n,b')
  return ()

-------------------------------------------------------------------------------
-- convenience methods for when your graph is a DAG

data DAG = Node Shape Text [DAG]

fromDAG :: DAG -> Dot ()
fromDAG = void . go
  where
    go (Node sh lbl []) = mkNode [Shape sh, Label lbl]
    go (Node sh lbl xs) = do
      n <- mkNode [Shape sh, Label lbl]
      ms <- mapM go xs
      mapM_ (mkEdge n) ms
      return n

-------------------------------------------------------------------------------
-- low-level string builders

edgeStmt :: NodeId -> NodeId -> Builder
edgeStmt a b = a' <> " -> " <> b' <> ";\n"
  where
    a' = LB.fromString $ show a
    b' = LB.fromString $ show b

nodeStmt :: NodeId -> [Attribute] -> Builder
nodeStmt n as = n' <> " [" <> as' <> "];\n"
  where
    n' = LB.fromString $ show n
    as' = mconcat $ List.intersperse "," $ map attrStr as

shapeStr :: Shape -> Builder
shapeStr Circle = "circle"
shapeStr Box = "box"

attrStr :: Attribute -> Builder
attrStr (Shape s) = "shape=" <> shapeStr s
attrStr (Label t) = 
  "label=\"" <> (LB.fromString $ escape $ Text.unpack t) <> "\""

escape :: [Char] -> [Char]
escape ('\\':xs) = '\\' : '\\' : escape xs
escape ('\"':xs) = '\\' : '\"' : escape xs
escape ('\n':xs) = '\\' : 'n' : escape xs
escape (x:xs) = x : escape xs
escape [] = []
