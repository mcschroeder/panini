module Panini.Syntax.QQ where  

import Data.Text qualified as Text
import Language.Haskell.TH.Quote
import Panini.Diagnostic
import Panini.Parser
import Panini.Pretty
import Prelude

panType :: QuasiQuoter
panType = QuasiQuoter {
  quoteExp = \s -> do
    case parseType "" (Text.strip $ Text.pack s) of
      Left e -> fail $ show $ group $ diagnosticMessage e
      Right ty -> dataToExpQ (const Nothing) ty
  
  , quotePat = undefined
  , quoteType = undefined
  , quoteDec = undefined
}
