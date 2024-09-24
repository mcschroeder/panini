module Panini.Pretty 
  ( Pretty(..)
  , showPretty
  , printPretty
  , (<\>), (<\\>), (<+>), PP.vcat, PP.vsep, PP.sep
  , PP.align, PP.hang, PP.group, PP.nest
  , PP.viaShow
  , concatWithOp
  , prettyTuple, prettyTupleTight
  , prettyList
  , prettySet, prettySetTight
  , prettyMap
  , parensIf
  , prettyL, prettyR
  , divider
  , module Panini.Pretty.Doc
  , module Panini.Pretty.Fixity
  , module Panini.Pretty.Render
  , module Panini.Pretty.Style
  , module Panini.Pretty.Symbols
  ) where

import Data.Char
import Data.HashSet (HashSet)
import Data.HashSet qualified as HashSet
import Data.List qualified as List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String
import Data.Text (Text)
import Data.Text qualified as Text
import Panini.Pretty.Doc
import Panini.Pretty.Fixity
import Panini.Pretty.Render
import Panini.Pretty.Style
import Panini.Pretty.Symbols
import Prelude
import Prettyprinter ((<+>))
import Prettyprinter qualified as PP

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

instance (Pretty a, Pretty b, Pretty c) => Pretty (a,b,c) where
  pretty (a,b,c) = listed lparen rparen [pretty a, pretty b, pretty c]

instance {-# OVERLAPPABLE #-} Pretty a => Pretty [a] where
  pretty xs = prettyList xs

instance Pretty a => Pretty (Set a) where
  pretty = prettySet . Set.toAscList

instance Pretty IntSet where
  pretty = prettySet . IntSet.toAscList

instance Pretty a => Pretty (HashSet a) where
  pretty = prettySet . HashSet.toList

instance (Pretty a, Pretty b) => Pretty (Map a b) where
  pretty = prettyMap . Map.toAscList

instance (Pretty a) => Pretty (IntMap a) where
  pretty = prettyMap . IntMap.toAscList

instance Pretty a => Pretty (Maybe a) where
  pretty Nothing = "Nothing"
  pretty (Just a) = pretty a

-- | A pretty version of 'show', mainly intended for debugging. Outputs Unicode
-- but does not output color. Use 'renderDoc' for any serious pretty printing.
showPretty :: Pretty a => a -> String
showPretty = Text.unpack . renderDoc o . pretty
  where 
    o = RenderOptions
          { styling    = Nothing
          , unicode    = True
          , fixedWidth = Nothing
          }

printPretty :: Pretty a => a -> IO ()
printPretty = putStrLn . showPretty

-------------------------------------------------------------------------------

-- | Insert a linebreak.
(<\>) :: Doc -> Doc -> Doc
a <\> b = a <> PP.line <> b

-- | Insert a hard linebreak.
(<\\>) :: Doc -> Doc -> Doc
a <\\> b = a <> PP.hardline <> b

concatWithOp :: Doc -> [Doc] -> Doc
concatWithOp op = PP.fillSep . List.intersperse op

prettyTuple :: Pretty a => [a] -> Doc
prettyTuple = listed lparen rparen . map pretty

prettyTupleTight :: Pretty a => [a] -> Doc
prettyTupleTight = parens . mconcat . List.intersperse comma . map pretty

prettyList :: Pretty a => [a] -> Doc
prettyList = PP.align . listed lbracket rbracket . map pretty

prettySet :: Pretty a => [a] -> Doc
prettySet = listed lbrace rbrace . map pretty

prettySetTight :: Pretty a => [a] -> Doc
prettySetTight = braces . mconcat . List.intersperse comma . map pretty

prettyMap :: (Pretty a, Pretty b) => [(a,b)] -> Doc
prettyMap = PP.align . listed lbrace rbrace . map (\(a,b) -> pretty a <+> mapsTo <+> pretty b)

listed :: Doc -> Doc -> [Doc] -> Doc
listed bra ket = PP.group . PP.encloseSep lp rp co
  where
    lp = PP.flatAlt (bra <> PP.space) bra
    rp = PP.flatAlt (PP.space <> ket) ket
    co = comma <> PP.space

parensIf :: Bool -> Doc -> Doc
parensIf x = if x then parens else id

prettyL :: (HasFixity a, HasFixity b, Pretty a) => b -> a -> Doc
prettyL p0 p1 = parensIf (p1 `needsParensLeftOf` p0) (pretty p1)

prettyR :: (HasFixity a, HasFixity b, Pretty a) => b -> a -> Doc
prettyR p0 p2 = parensIf (p2 `needsParensRightOf` p0) (pretty p2)

-------------------------------------------------------------------------------

divider :: Doc -> Maybe (Either String String) -> Doc
divider d label = PP.column $ \c -> PP.pageWidth $ \pw -> case label of
  Just (Left  l) -> pretty l <+> div_ (getW pw - c - length l - 1)
  Just (Right l) ->              div_ (getW pw - c - length l - 1) <+> pretty l
  Nothing        ->              div_ (getW pw - c)  
 where
  div_ w = mconcat $ replicate w d
  getW = \case
    PP.AvailablePerLine w _ -> w
    PP.Unbounded            -> 80
