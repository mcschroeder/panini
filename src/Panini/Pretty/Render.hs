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
  LT.toStrict . LB.toLazyText . go [] . treeForm . PP.layoutSmart layoutOpt
 where  
  layoutOpt = PP.defaultLayoutOptions { PP.layoutPageWidth = pw }
  pw = maybe PP.Unbounded (\w -> PP.AvailablePerLine w 1) o.fixedWidth

  go _ STEmpty       = mempty
  go _ (STChar c)    = LB.singleton c
  go _ (STText _ t)  = LB.fromText t
  go s (STLine i)    = suspend s $ LB.singleton '\n' <> spaces i
  go s (STConcat ds) = mconcat $ map (go s) ds

  go s (STAnn a d)
    | ASCII t <- a, not o.unicode = LB.fromText t
    | Just f <- o.styling         = goColor (f a) s d
    | otherwise                   = go s d

  goColor a s d = case s of
    []  -> sgr (s2sgr a)           <> go (a:s) d <> sgr [Reset]
    b:_ -> sgr (s2sgr $ sDiff b a) <> go (a:s) d <> sgr (s2sgr $ sDiff a b)

  sgr [] = mempty
  sgr cs = LB.fromString $ setSGRCode cs

  spaces i = LB.fromText $ Text.replicate i $ Text.singleton ' '

  suspend s d
    | null s         = d
    | null o.styling = d
    | otherwise      = sgr [Reset] <> d <> sgr (concatMap s2sgr s)

sDiff :: Style -> Style -> Style
sDiff old new = Style 
  { bold      = diff old.bold new.bold
  , underline = diff old.underline new.underline
  , fgColor   = diff old.fgColor new.fgColor
  , bgColor   = diff old.bgColor new.bgColor
  }
  where
    diff o n = if o == n then Nothing else n

s2sgr :: Style -> [SGR]
s2sgr new = catMaybes
  [ SetConsoleIntensity <$> (if' new.bold BoldIntensity NormalIntensity)
  , SetUnderlining      <$> (if' new.underline SingleUnderline NoUnderline)
  , uncurry (SetColor Foreground) <$> new.fgColor
  , uncurry (SetColor Background) <$> new.bgColor
  ]
  where
    if' (Just True ) a _ = Just a
    if' (Just False) _ b = Just b
    if' Nothing      _ _ = Nothing
