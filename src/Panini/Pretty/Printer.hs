module Panini.Pretty.Printer 
  ( Pretty(..)
  , Doc
  , (<+>), (<\>), (<\\>), group, nest, annotate, Ann(..)
  , RenderOptions(..)
  , renderDoc
  , showPretty
  , showPrettyDoc
  , HasFixity(..), Fixity(..), Associativity(..)
  , needsParensLeftOf, needsParensRightOf, needsParensPrefixedBy
  , parensIf, prettyL, prettyR
  , viaShow
  , prettyKeyword
  , prettySymbol
  , prettyKeywordSymbol
  , PP.parens
  , PP.braces
  , PP.brackets
  , PP.vcat
  , PP.space
  , PP.concatWith
  , symDot, symDotDot, symColon, symArrow
  , symAnd, symOr, symNeg, symImplies, symIff, symAll, symExists
  , symNe, symEq, symLe, symLt, symGe, symGt
  , symKappa
  , prettyTuple  
  ) where

import Control.Monad
import Data.Text (Text)
import Data.Text qualified as Text
import Prelude
import Prettyprinter hiding (Pretty(..), Doc)
import Prettyprinter qualified as PP
import Prettyprinter.Render.Util.SimpleDocTree
import System.Console.ANSI
import Data.List qualified as List

-------------------------------------------------------------------------------

showPretty :: Pretty a => a -> String
showPretty = Text.unpack . renderDoc opts . pretty
  where
    opts = RenderOptions 
      { ansiColors = True
      , unicodeSymbols = True
      , fixedWidth = Nothing 
      }

showPrettyDoc :: Doc -> String
showPrettyDoc = Text.unpack . renderDoc opts
  where
    opts = RenderOptions 
      { ansiColors = True
      , unicodeSymbols = True
      , fixedWidth = Nothing 
      }

-------------------------------------------------------------------------------

type Doc = PP.Doc Ann

data Ann = Keyword | Symbol | Predicate | Message | AError | Margin

-- | Our custom pretty-printing class, fixing `Doc` annotations to `Ann`.
class Pretty a where
  pretty :: a -> Doc

instance Pretty Text where pretty = PP.pretty
instance Pretty String where pretty = PP.pretty
instance Pretty Char where pretty = PP.pretty
instance Pretty Integer where pretty = PP.pretty
instance Pretty Int where pretty = PP.pretty

instance (Pretty a, Pretty b) => Pretty (a, b) where 
  pretty (a,b) = "(" <> pretty a <> "," <+> pretty b <> ")"

-------------------------------------------------------------------------------

data RenderOptions = RenderOptions 
  { ansiColors     :: Bool
  , unicodeSymbols :: Bool
  , fixedWidth     :: Maybe Int
  }

renderDoc :: RenderOptions -> Doc -> Text
renderDoc opts = 
  renderSimplyDecorated renderT renderA . treeForm . layoutSmart layoutOpt
 where
  layoutOpt = defaultLayoutOptions { layoutPageWidth = pw }
  pw = maybe Unbounded (\w -> AvailablePerLine w 1) opts.fixedWidth
  renderT = id
  renderA = liftM2 (.) 
      (if opts.ansiColors     then colorize  else const id) 
      (if opts.unicodeSymbols then unicodify else const id)

  colorize = \case
    Keyword   -> sgr [SetConsoleIntensity BoldIntensity]
    Predicate -> sgr [SetColor Foreground Vivid Blue]
    Message   -> sgr [SetConsoleIntensity BoldIntensity]
    AError     -> sgr [SetColor Foreground Vivid Red]
    Margin    -> sgr [SetColor Foreground Dull Blue]
    _         -> id
 
  sgr c t = Text.pack (setSGRCode c) <> t <> Text.pack (setSGRCode [Reset])

  unicodify Symbol = \case
    "/\\" -> "∧"
    "\\/" -> "∨"
    ">="  -> "≥"
    "<="  -> "≤"
    "~"   -> "¬"
    "/="  -> "≠"
    "==>" -> "⇒"
    "<=>" -> "⇔"
    "->"  -> "→"
    "\\"  -> "λ"
    "forall " -> "∀"  -- note the extra space
    "exists " -> "∃"  -- note the extra space
    x     -> x
  unicodify Margin = Text.replace "|" "│"
  unicodify _ = id

-------------------------------------------------------------------------------

symDot, symDotDot, symColon, symArrow :: Doc
symDot = "."
symDotDot = ".."
symColon = ":"
symArrow = "→"

symAnd, symOr, symNeg, symImplies, symIff, symAll, symExists :: Doc
symAnd = "∧"
symOr = "∨"
symNeg = "¬"
symImplies = "⇒"
symIff = "⇔"
symAll = "∀"
symExists = "∃"

symNe, symEq, symLe, symLt, symGe, symGt :: Doc
symNe = "≠"
symEq = "="
symLe = "≤"
symLt = "<"
symGe = "≥"
symGt = ">"

symKappa :: Doc
symKappa = "κ"

-------------------------------------------------------------------------------

prettyKeyword :: Text -> Doc
prettyKeyword = annotate Keyword . pretty

prettySymbol :: Text -> Doc
prettySymbol = annotate Symbol . pretty

prettyKeywordSymbol :: Text -> Doc
prettyKeywordSymbol = annotate Keyword . annotate Symbol . pretty

-- | Inserts a linebreak.
(<\>) :: Doc -> Doc -> Doc
a <\> b = a <> line <> b

-- | Inserts a hard linebreak.
(<\\>) :: Doc -> Doc -> Doc
a <\\> b = a <> hardline <> b

-------------------------------------------------------------------------------

prettyTuple :: Pretty a => [a] -> Doc
prettyTuple = parens . mconcat . List.intersperse "," . map pretty

-------------------------------------------------------------------------------

class HasFixity a where
  fixity :: a -> Fixity

data Fixity = Prefix | Infix Associativity Int
data Associativity = LeftAss | RightAss | NoAss

parensIf :: Bool -> Doc -> Doc
parensIf x = if x then parens else id

needsParensLeftOf :: (HasFixity a, HasFixity b) => a -> b -> Bool
needsParensLeftOf a o = case (fixity a, fixity o) of
  (Infix _ lhs, Infix LeftAss  op) -> lhs <  op
  (Infix _ lhs, Infix NoAss    op) -> lhs <= op
  (Infix _ lhs, Infix RightAss op) -> lhs <= op
  _                                -> False

needsParensRightOf :: (HasFixity a, HasFixity b) => a -> b -> Bool
needsParensRightOf a o = case (fixity o, fixity a) of
  (Infix LeftAss  op, Infix _ rhs) -> op >= rhs
  (Infix NoAss    op, Infix _ rhs) -> op >= rhs
  (Infix RightAss op, Infix _ rhs) -> op >  rhs
  _                                -> False

needsParensPrefixedBy :: (HasFixity a, HasFixity b) => a -> b -> Bool
needsParensPrefixedBy a o = case (fixity o, fixity a) of
  (Prefix, Prefix) -> False
  _                -> True

prettyL :: (HasFixity a, HasFixity b, Pretty a) => b -> a -> Doc
prettyL p0 p1 = parensIf (p1 `needsParensLeftOf` p0) (pretty p1)

prettyR :: (HasFixity a, HasFixity b, Pretty a) => b -> a -> Doc
prettyR p0 p2 = parensIf (p2 `needsParensRightOf` p0) (pretty p2)
