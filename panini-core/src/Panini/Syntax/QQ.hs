module Panini.Syntax.QQ where  

import Language.Haskell.TH.Quote
import Panini.Parser
import Data.Text qualified as Text
import Prelude
import Panini.Pretty

-- TODO: improve error message
panType :: QuasiQuoter
panType = QuasiQuoter {
  quoteExp = \s -> do
    case parseType "" (Text.strip $ Text.pack s) of
      Left (ParserError err _) -> fail $ showPretty err
      Right ty -> dataToExpQ (const Nothing) ty
  
  , quotePat = undefined
  , quoteType = undefined
  , quoteDec = undefined
}
