module Panini.Pretty.Symbols where

import Data.Char
import Data.String
import Panini.Pretty.Doc
import Prelude

-------------------------------------------------------------------------------

orASCII :: String -> String -> Doc
orASCII d t = ann (ASCII (fromString t)) (fromString d)

-------------------------------------------------------------------------------

parens :: Doc -> Doc
parens d = lparen <> d <> rparen

brackets :: Doc -> Doc
brackets d = lbracket <> d <> rbracket

braces :: Doc -> Doc
braces d = lbrace <> d <> rbrace

lparen, rparen :: Doc
lparen = ann (Bracket OpenBra)  "("
rparen = ann (Bracket CloseBra) ")"

lbracket, rbracket :: Doc
lbracket = ann (Bracket OpenBra)  "["
rbracket = ann (Bracket CloseBra) "]"

lbrace, rbrace :: Doc
lbrace = ann (Bracket OpenBra)  "{"
rbrace = ann (Bracket CloseBra) "}"

-------------------------------------------------------------------------------

subscript :: Int -> Doc
subscript i = fromString (map go ds) `orASCII` (fromString ds)
  where
    ds = show i
    go c = if isDigit c then chr (ord c + 8272) else c

superscript :: Int -> Doc
superscript i = fromString (map go ds) `orASCII` (fromString ds)
  where
    ds = show i
    go c = if isDigit c then "⁰¹²³⁴⁵⁶⁷⁸⁹" !! digitToInt c else c

-------------------------------------------------------------------------------

comma, mid, dot, colon, arrow, mapsTo :: Doc 
comma  = ann Separator ","
mid    = ann Separator "|"
dot    = ann Separator "."
colon  = ann Separator ":"
arrow  = "→" `orASCII` "->"
mapsTo = "↦" `orASCII` "|->"

emptySet, setMinus :: Doc
emptySet = "∅" `orASCII` "{}"
setMinus = "∖" `orASCII` "\\"

symDivH, symDivH2, symDivV, symDivDiag :: Doc
symDivH    = "─" `orASCII` "-"
symDivH2   = "╌" `orASCII` "-"
symDivV    = "│" `orASCII` "|"
symDivDiag = "╱" `orASCII` "/"

wedge, vee, symNeg, symImplies, symIff, symAll, symExists :: Doc
wedge      = "∧" `orASCII` "/\\"
vee        = "∨" `orASCII` "\\/"
symNeg     = "¬" `orASCII` "~"
symImplies = "⇒" `orASCII` "==>"
symIff     = "⇔" `orASCII` "<==>"
symAll     = "∀" `orASCII` "forall "
symExists  = "∃" `orASCII` "exists "

symNe, symEq, symLe, symLt, symGe, symGt, symIn, symNi :: Doc
symNe = "≠" `orASCII` "/="
symEq = "="
symLe = "≤" `orASCII` "<="
symLt = "<"
symGe = "≥" `orASCII` ">="
symGt = ">"
symIn = "∈" `orASCII` "\\in"
symNi = "∉" `orASCII` "\\notin"

-- TODO: these replacements could cause massive confusion in string grammars
lambda, kappa, sigma, bigSigma, epsilon :: Doc
lambda = "λ" `orASCII` "\\"
kappa  = "κ" `orASCII` "k"
sigma  = "σ" `orASCII` "s"
bigSigma = "Σ" `orASCII` "S"
epsilon = "ε" `orASCII` "e"

symTUnit, symTBool, symTNat, symTInt, symTString :: Doc
symTUnit   = ann (Identifier TypeIdent) $ "𝟙" `orASCII` "unit"
symTBool   = ann (Identifier TypeIdent) $ "𝔹" `orASCII` "bool"
symTNat    = ann (Identifier TypeIdent) $ "ℕ" `orASCII` "nat"
symTInt    = ann (Identifier TypeIdent) $ "ℤ" `orASCII` "int"
symTString = ann (Identifier TypeIdent) $ "𝕊" `orASCII` "string"

symUnit, symTrue, symFalse :: Doc
symUnit  = ann (Literal OtherLit) "unit"
symTrue  = ann (Literal OtherLit) "true"
symFalse = ann (Literal OtherLit) "false"

symTop, symBot, symInf :: Doc
symTop = "⊤" `orASCII` "top"
symBot = "⊥" `orASCII` "bot"
symInf = "∞" `orASCII` "inf"
