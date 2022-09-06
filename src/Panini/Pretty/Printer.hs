module Panini.Pretty.Printer 
  ( Pretty(..)
  , showPretty
  , Doc
  , Ann(..), IdentKind(..), LitKind(..)
  , Style(..), defaultStyling
  , (<+>), (<\>), (<\\>), PP.vcat, PP.vsep, PP.sep
  , PP.align, PP.hang, PP.group, PP.nest
  , PP.viaShow
  , keyword, literal, identifier, aMessage, anError, marginalia, highlight
  , orASCII
  , subscript
  , concatWithOp
  , prettyTuple, prettyList, prettyMap
  , parens, brackets, braces
  , symDot, symDotDot, symColon, symArrow, symMapsTo
  , symAnd, symOr, symNeg, symImplies, symIff, symAll, symExists
  , symNe, symEq, symLe, symLt, symGe, symGt
  , symLambda, symKappa
  , symUnit, symBool, symNat, symInt, symString
  , HasFixity(..), Fixity(..), Associativity(..)
  , needsParensLeftOf, needsParensRightOf, needsParensPrefixedBy
  , parensIf, prettyL, prettyR
  , RenderOptions(..), renderDoc_, renderDoc  
  ) where

import Data.Char
import Data.List qualified as List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Lazy qualified as LT
import Data.Text.Lazy.Builder qualified as LB
import Prelude
import Prettyprinter ((<+>))
import Prettyprinter qualified as PP
import Prettyprinter.Render.Util.SimpleDocTree
import System.Console.ANSI

-------------------------------------------------------------------------------

class Pretty a where
  pretty :: a -> Doc

instance Pretty Doc     where pretty = id
instance Pretty Text    where pretty = PP.pretty
instance Pretty String  where pretty = PP.pretty
instance Pretty Char    where pretty = PP.pretty
instance Pretty Integer where pretty = PP.pretty
instance Pretty Int     where pretty = PP.pretty

instance (Pretty a, Pretty b) => Pretty (a,b) where
  pretty (a,b) = listed lparen rparen [pretty a, pretty b]

instance {-# OVERLAPPABLE #-} Pretty a => Pretty [a] where
  pretty xs = prettyList xs

instance Pretty a => Pretty (Set a) where
  pretty = prettySet . Set.toAscList

instance (Pretty a, Pretty b) => Pretty (Map a b) where
  pretty = prettyMap . Map.toAscList

instance Pretty a => Pretty (Maybe a) where
  pretty Nothing = "Nothing"
  pretty (Just a) = pretty a

-- | A pretty version of 'show', mainly intended for debugging.
-- Use 'renderDoc' for any serious pretty printing.
showPretty :: Pretty a => a -> String
showPretty = Text.unpack . renderDoc_ . pretty

-------------------------------------------------------------------------------

type Doc = PP.Doc Ann

data Ann 
  = Keyword               -- ^ Keywords, e.g., @let@ or @assume@.
  | Identifier IdentKind  -- ^ Identifiers, like variables and types.
  | Literal LitKind       -- ^ Literals, e.g., integers or strings.
  | Bracket BraKind       -- ^ Matched nesting symbols, e.g., parentheses.
  | Separator             -- ^ Syntactic separators, e.g., commas.
  | Highlight             -- ^ Highlighted piece of syntax, something notable.
  | Message               -- ^ Any kind of message from the compiler.
  | Error                 -- ^ Something erroneous.
  | Margin                -- ^ Marginalia, like line numbers and such.
  | ASCII Text            -- ^ Alternative ASCII version of a Unicode symbol.
  deriving stock (Eq, Show, Read)

data IdentKind
  = VarIdent   -- ^ variables
  | TypeIdent  -- ^ types
  deriving stock (Eq, Show, Read)

data LitKind 
  = NumberLit  -- ^ integers, reals, etc.
  | StringLit  -- ^ strings and characters
  | OtherLit   -- ^ booleans, unit, etc.
  deriving stock (Eq, Show, Read)

data BraKind = OpenBra | CloseBra
  deriving stock (Eq, Show, Read)

data Style = Style
  { bold      :: Maybe Bool
  , underline :: Maybe Bool
  , fgColor   :: Maybe (ColorIntensity, Color)
  , bgColor   :: Maybe (ColorIntensity, Color)
  }
  deriving stock (Eq, Show, Read)

defaultStyling :: Ann -> Style
defaultStyling = \case
  Keyword -> s { bold = Just True }
  Identifier x -> case x of
    VarIdent  -> s { fgColor = Just (Dull, Magenta) }
    TypeIdent -> s { fgColor = Just (Vivid, Blue) }
  Literal x -> case x of
    NumberLit -> s { fgColor = Just (Dull, Red) }
    StringLit -> s { fgColor = Just (Dull, Red) }
    OtherLit  -> s { fgColor = Just (Dull, Red) }
  Bracket _ -> s { fgColor = Just (Vivid, Black)}
  Separator -> s { fgColor = Just (Vivid, Black)}
  Highlight -> s { bgColor = Just (Vivid, Yellow)}
  Message -> s { bold = Just True }
  Error   -> s { bold = Just True, fgColor = Just (Vivid, Red) }
  Margin  -> s { fgColor = Just (Dull, Blue) }
  _       -> s
  where
    s = Style Nothing Nothing Nothing Nothing

-------------------------------------------------------------------------------

-- | Inserts a linebreak.
(<\>) :: Doc -> Doc -> Doc
a <\> b = a <> PP.line <> b

-- | Inserts a hard linebreak.
(<\\>) :: Doc -> Doc -> Doc
a <\\> b = a <> PP.hardline <> b

keyword :: Text -> Doc
keyword = PP.annotate Keyword . pretty

literal :: LitKind -> Doc -> Doc
literal = PP.annotate . Literal

identifier :: IdentKind -> Doc -> Doc
identifier = PP.annotate . Identifier

aMessage :: Doc -> Doc
aMessage = PP.annotate Message

anError :: Doc -> Doc
anError = PP.annotate Error

marginalia :: Doc -> Doc
marginalia = PP.annotate Margin

highlight :: Doc -> Doc
highlight = PP.annotate Highlight

orASCII :: Doc -> Text -> Doc
orASCII d t = PP.annotate (ASCII t) d

subscript :: Int -> Doc
subscript i = fromString (map go ds) `orASCII` (fromString ds)
  where
    ds = show i
    go c = if isDigit c then chr (ord c + 8272) else c

concatWithOp :: Doc -> [Doc] -> Doc
concatWithOp op = PP.fillSep . List.intersperse op

prettyTuple :: Pretty a => [a] -> Doc
prettyTuple = listed lparen rparen . map pretty

prettyList :: Pretty a => [a] -> Doc
prettyList = listed lbracket rbracket . map pretty

prettySet :: Pretty a => [a] -> Doc
prettySet = listed lbrace rbrace . map pretty

prettyMap :: (Pretty a, Pretty b) => [(a,b)] -> Doc
prettyMap = listed lbrace rbrace . map (\(a,b) -> pretty a <> " ↦ " <> pretty b)

listed :: Doc -> Doc -> [Doc] -> Doc
listed bra ket = PP.group . PP.encloseSep lp rp co
  where
    lp = PP.flatAlt (bra <> PP.space) bra
    rp = PP.flatAlt (PP.space <> ket) ket
    co = symComma <> PP.space

parens :: Doc -> Doc
parens d = lparen <> d <> rparen

brackets :: Doc -> Doc
brackets d = lbracket <> d <> rbracket

braces :: Doc -> Doc
braces d = lbrace <> d <> rbrace

lparen, rparen :: Doc
lparen = PP.annotate (Bracket OpenBra) "("
rparen = PP.annotate (Bracket CloseBra) ")"

lbracket, rbracket :: Doc
lbracket = PP.annotate (Bracket OpenBra) "["
rbracket = PP.annotate (Bracket CloseBra) "]"

lbrace, rbrace :: Doc
lbrace = PP.annotate (Bracket OpenBra) "{"
rbrace = PP.annotate (Bracket CloseBra) "}"

-------------------------------------------------------------------------------

symComma, symDot, symDotDot, symColon, symArrow, symMapsTo :: Doc
symComma  = PP.annotate Separator ","
symDot    = "."
symDotDot = ".."
symColon  = ":"
symArrow  = "→" `orASCII` "->"
symMapsTo = "↦" `orASCII` "|->"

symAnd, symOr, symNeg, symImplies, symIff, symAll, symExists :: Doc
symAnd     = "∧" `orASCII` "/\\"
symOr      = "∨" `orASCII` "\\/"
symNeg     = "¬" `orASCII` "~"
symImplies = "⇒" `orASCII` "==>"
symIff     = "⇔" `orASCII` "<==>"
symAll     = "∀" `orASCII` "forall "
symExists  = "∃" `orASCII` "exists "

symNe, symEq, symLe, symLt, symGe, symGt :: Doc
symNe = "≠" `orASCII` "/="
symEq = "="
symLe = "≤" `orASCII` "<="
symLt = "<"
symGe = "≥" `orASCII` ">="
symGt = ">"

symLambda, symKappa :: Doc
symLambda = "λ" `orASCII` "\\"
symKappa  = "κ" `orASCII` "k"

symUnit, symBool, symNat, symInt, symString :: Doc
symUnit = "𝟙" `orASCII` "unit"
symBool = "𝔹" `orASCII` "bool"
symNat = "ℕ" `orASCII` "nat"
symInt = "ℤ" `orASCII` "int"
symString = "𝕊" `orASCII` "string"

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

-------------------------------------------------------------------------------

data RenderOptions = RenderOptions 
  { styling    :: Maybe (Ann -> Style)
  , unicode    :: Bool
  , fixedWidth :: Maybe Int
  }

renderDoc_ :: Doc -> Text
renderDoc_ = renderDoc RenderOptions
  { styling    = Just defaultStyling
  , unicode    = True
  , fixedWidth = Nothing
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
  go _ (STLine i)    = LB.singleton '\n' <> spaces i
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
