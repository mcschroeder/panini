module Panini.Pretty.Style where

import Panini.Pretty.Doc
import Prelude
import System.Console.ANSI

-------------------------------------------------------------------------------

defaultStyling :: Ann -> Style
defaultStyling = \case
  Keyword              -> bold True
  Identifier VarIdent  -> fgColor Dull Magenta
  Identifier TypeIdent -> fgColor Vivid Blue
  Literal _            -> fgColor Dull Red
  Bracket _            -> fgColor Vivid Black
  Separator            -> fgColor Vivid Black
  Highlight            -> bgColor Vivid Yellow
  Message              -> bold True
  Error                -> bold True <> fgColor Vivid Red
  Success              -> bold True <> fgColor Vivid Green
  Margin               -> fgColor Dull Blue
  ASCII _              -> mempty 
  NormalWeight         -> bold False

-------------------------------------------------------------------------------

bold :: Bool -> Style
bold b = mempty { _bold = Set b }

underline :: Bool -> Style
underline u = mempty { _underline = Set u }

fgColor :: ColorIntensity -> Color -> Style
fgColor i c = mempty { _fgColor = Set (i,c) }

bgColor :: ColorIntensity -> Color -> Style
bgColor i c = mempty { _bgColor = Set (i,c) }

-------------------------------------------------------------------------------

data Style = Style
  { _bold      :: Attribute Bool
  , _underline :: Attribute Bool
  , _fgColor   :: Attribute (ColorIntensity,Color)
  , _bgColor   :: Attribute (ColorIntensity,Color)
  }
  deriving stock (Eq, Ord, Show, Read)

instance Semigroup Style where
  a <> b = Style { _bold      = a._bold      <> b._bold
                 , _underline = a._underline <> b._underline
                 , _fgColor   = a._fgColor   <> b._fgColor
                 , _bgColor   = a._bgColor   <> b._bgColor
                 }

instance Monoid Style where
  mempty = Style Leave Leave Leave Leave

instance Delta Style where
  delta a b = Style { _bold      = delta a._bold      b._bold
                    , _underline = delta a._underline b._underline
                    , _fgColor   = delta a._fgColor   b._fgColor
                    , _bgColor   = delta a._bgColor   b._bgColor
                    }

toSGR :: Style -> [SGR]
toSGR a = mconcat
  [ case a._bold of
      Set True  -> [SetConsoleIntensity BoldIntensity]
      Set False -> [SetConsoleIntensity NormalIntensity]
      Leave     -> []
  , case a._underline of
      Set True  -> [SetUnderlining SingleUnderline]
      Set False -> [SetUnderlining NoUnderline]
      Leave     -> []
  , case a._fgColor of
      Set (i,c) -> [SetColor Foreground i c]
      Leave     -> []
  , case a._bgColor of
      Set (i,c) -> [SetColor Background i c]
      Leave     -> []
  ]

-------------------------------------------------------------------------------

data Attribute a = Set a | Leave
  deriving stock (Eq, Ord, Read, Show)

instance Semigroup (Attribute a) where
  a <> Leave = a
  _ <> Set b = Set b

instance Monoid (Attribute a) where
  mempty = Leave

instance Eq a => Delta (Attribute a) where
  delta a       Leave            = a
  delta (Set a) (Set b) | a == b = Leave
  delta _       (Set b)          = Set b

-------------------------------------------------------------------------------

class Delta a where
  delta :: a -> a -> a
