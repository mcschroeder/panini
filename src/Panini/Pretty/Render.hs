module Panini.Pretty.Render where

import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Lazy qualified as LT
import Data.Text.Lazy.Builder qualified as LB
import Panini.Pretty.Doc
import Panini.Pretty.Style
import Prelude
import Prettyprinter qualified as PP
import Prettyprinter.Render.Util.SimpleDocTree
import System.Console.ANSI

-------------------------------------------------------------------------------

data RenderOptions = RenderOptions 
  { styling    :: Maybe (Ann -> Style)
  , unicode    :: Bool
  , fixedWidth :: Maybe Int
  }

renderDoc :: RenderOptions -> Doc -> Text
renderDoc o =
  LT.toStrict . LB.toLazyText . go0 . treeForm . PP.layoutSmart layoutOpt
 where  
  layoutOpt = PP.defaultLayoutOptions { PP.layoutPageWidth = pw }
  pw = maybe PP.Unbounded (\w -> PP.AvailablePerLine w 1) o.fixedWidth

  go0 t
    | null o.styling = go mempty t
    | otherwise      = sgr [Reset] <> go mempty t <> sgr [Reset]

  go _ STEmpty       = mempty
  go _ (STChar c)    = LB.singleton c
  go _ (STText _ t)  = LB.fromText t
  go s (STLine i)    = suspendStyle s $ LB.singleton '\n' <> spaces i
  go s (STConcat ds) = mconcat $ map (go s) ds

  go s (STAnn (ASCII t) d) 
    | not o.unicode = LB.fromText t
    | otherwise     = go s d

  go s (STAnn a d)
    | Just f <- o.styling = goStyle s (s <> f a) d
    | otherwise           = go s d

  goStyle s0 s1 d = 
    sgr (toSGR $ delta s0 s1) <> go s1 d <> sgr (Reset : toSGR s0)

  suspendStyle s t
    | s /= mempty = sgr [Reset] <> t <> sgr (toSGR s)
    | otherwise   = t

  sgr [] = mempty
  sgr cs = LB.fromString $ setSGRCode cs

  spaces i = LB.fromText $ Text.replicate i $ Text.singleton ' '
