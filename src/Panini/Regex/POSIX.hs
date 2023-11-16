module Panini.Regex.POSIX where

import Data.List (intersperse)
import Panini.Regex
import Panini.Regex.CharSet
import Prelude

-- TODO: print character ranges
-- TODO: print pre-defined POSIX character classes
-- TODO: proper escaping
-- TODO: parse EREs

-------------------------------------------------------------------------------

printERE :: Regex -> String
printERE = go False
 where
  go o = \case
    Lit c        -> goCS c
    Word s       -> s
    Plus xs      -> parens o $ mconcat $ intersperse "|" $ map (go False) xs
    Times xs     -> mconcat $ map (go True) xs
    Star (Lit c) -> goCS c <> "*"
    Star x       -> parens True (go False x) <> "*"
    Opt (Lit c)  -> goCS c <> "*"
    Opt x        -> parens True (go False x) <> "?"

  goCS (CharSet b s) = case (b, intSetToCharList s) of
    (True , [x]) -> [x]
    (True , xs ) -> "[" ++ xs ++ "]"
    (False, [] ) -> "."
    (False, xs ) -> "[^" ++ xs ++ "]"

  parens True  s = "(" ++ s ++ ")"
  parens False s = s
