module Panini.Regex.SMT where

import Panini.Regex.CharSet qualified as CS
import Panini.Regex.Type
import Panini.SMT.RegLan qualified as SMT
import Data.IntSet qualified as IS
import Prelude

-- TODO: fromRegLan

-------------------------------------------------------------------------------

toRegLan :: Regex -> SMT.RegLan
toRegLan = \case
  Zero -> SMT.None
  One -> SMT.ToRe ""    
  AnyChar -> SMT.AllChar
  All -> SMT.All
  Lit c -> case CS.fromCharSet c of
    (True, a) -> charsetToRegLan a
    (False, a) -> SMT.Diff SMT.AllChar (charsetToRegLan a)
  Plus rs -> foldr1 SMT.Union (map toRegLan rs)
  Times rs -> foldr1 SMT.Conc (map toRegLan rs)
  Star r -> SMT.Star (toRegLan r)
  Opt r -> SMT.Opt (toRegLan r)
 where  
  charsetToRegLan cs = case (IS.size cs, IS.findMin cs, IS.findMax cs) of
    (1,l,_) -> SMT.ToRe [toEnum @Char l]
    (n,l,u) | fromEnum u - fromEnum l + 1 == n -> SMT.Range (toEnum @Char l) (toEnum @Char u)
    _ -> foldr1 SMT.Union $ map (SMT.ToRe . pure . toEnum @Char) $ IS.toList cs
