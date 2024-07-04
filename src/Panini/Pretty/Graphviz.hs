module Panini.Pretty.Graphviz where

import Control.Monad
import Data.List qualified as List
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Lazy qualified as LT
import Data.Text.Lazy.Builder (Builder)
import Data.Text.Lazy.Builder qualified as LB
import Panini.Pretty (Doc, showPretty)
import Prelude
import System.IO.Error
import System.IO.Unsafe
import System.Process

-------------------------------------------------------------------------------

class Graphviz a where  
  dot :: a -> DOT

renderDOT :: Graphviz a => a -> Text
renderDOT x = case dot x of
  Digraph stmts -> LT.toStrict $ LB.toLazyText $ digraphStr stmts

renderGraph :: Graphviz a => FilePath -> a -> IO ()
renderGraph f x = do  
  let d' = Text.unpack $ renderDOT x
  let args = ["-Tsvg", "-o" ++ f]
  void $ readProcess "dot" args d'

traceGraph :: Graphviz a => FilePath -> a -> a
traceGraph f x = unsafePerformIO $ do
  putStrLn $ "rendering graph to " ++ f
  catchIOError (renderGraph f x) print
  return x

-------------------------------------------------------------------------------

data DOT = Digraph [Statement]

data Statement
  = Node Id [Attribute]
  | Edge Id Id [Attribute]
  | Subgraph Id [Attribute] [Statement]

type Id = String

data Attribute
  = Shape Shape
  | Style Style
  | Label Doc
  | Other String String

data Shape 
  = Circle 
  | Box 
  | Diamond 
  | Triangle 
  | InvertedTriangle 
  | Ellipse 
  | Record
  | Point
  | None

data Style
  = Dashed      -- ^ nodes, edges, clusters
  | Dotted      -- ^ nodes, edges, clusters
  | Solid       -- ^ nodes, edges, clusters
  | Invisible   -- ^ nodes, edges, clusters
  | Bold        -- ^ nodes, edges, clusters
  | Tapered     -- ^ edges
  | Filled      -- ^ nodes, clusters
  | Striped     -- ^ nodes, clusters
  | Wedged      -- ^ nodes
  | Diagonals   -- ^ nodes
  | Rounded     -- ^ nodes, clusters
  | Radial      -- ^ nodes, clusters, graphs

-------------------------------------------------------------------------------

digraphStr :: [Statement] -> Builder
digraphStr xs = "digraph {\n" <> foldMap stmtStr xs <> "}\n"

stmtStr :: Statement -> Builder
stmtStr = \case
  Node n as -> id_ n <> attrStrs as <> ";\n"
  Edge a b as -> id_ a <> "->" <> id_ b <> attrStrs as <> ";\n"
  Subgraph c as xs -> "subgraph " <> id_ c <> "{\n" <> 
                        foldMap (\a -> attrStr a <> ";\n") as <> 
                        foldMap stmtStr xs <> 
                      "}\n"
 where
  id_ = LB.fromString

attrStrs :: [Attribute] -> Builder
attrStrs xs = "[" <> (mconcat $ List.intersperse "," $ map attrStr xs) <> "]"

attrStr :: Attribute -> Builder
attrStr = \case
  Shape s -> "shape=" <> shapeStr s
  Style s -> "style=" <> styleStr s
  Label t -> "label=\"" <> (LB.fromString $ escape $ showPretty t) <> "\""
  Other k v -> LB.fromString k <> "=\"" <> (LB.fromString $ escape v) <> "\""

shapeStr :: Shape -> Builder
shapeStr = \case
  Circle            -> "circle"
  Box               -> "box"
  Diamond           -> "diamond"
  Triangle          -> "triangle"
  InvertedTriangle  -> "invtriangle"
  Ellipse           -> "ellipse"
  Record            -> "record"
  Point             -> "point"
  None              -> "none"

styleStr :: Style -> Builder
styleStr = \case
  Dashed    -> "dashed"
  Dotted    -> "dotted"
  Solid     -> "solid"
  Invisible -> "invis"
  Bold      -> "bold"
  Tapered   -> "tapered"
  Filled    -> "filled"
  Striped   -> "striped"
  Wedged    -> "wedged"
  Diagonals -> "diagonals"
  Rounded   -> "rounded"
  Radial    -> "radial"

escape :: [Char] -> [Char]
escape ('\\':x:xs) 
  | x `elem` ("lr|{}<>" :: String) = '\\' : x : escape xs
  | otherwise = '\\' : '\\' : escape xs
escape ('\"':xs) = '\\' : '\"' : escape xs
escape ('\n':xs) = '\\' : 'n' : escape xs
escape (x:xs) = x : escape xs
escape [] = []
