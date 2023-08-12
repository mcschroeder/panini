module Panini.Pretty.Style where

import Panini.Pretty.Doc
import Prelude
import System.Console.ANSI

-------------------------------------------------------------------------------

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
  Success -> s { bold = Just True, fgColor = Just (Vivid, Green) }
  Margin  -> s { fgColor = Just (Dull, Blue) }
  _       -> s
  where
    s = Style Nothing Nothing Nothing Nothing


-- TODO: add light/dark styles
