module Panini.Pretty.Doc where

import Data.Text (Text)
import Prelude
import Prettyprinter qualified as PP

-------------------------------------------------------------------------------

-- | Pretty-printed documents, annotated with highlighting information.
type Doc = PP.Doc Ann

-- | Add annotation to document.
ann :: Ann -> Doc -> Doc
ann = PP.annotate

-------------------------------------------------------------------------------

-- TODO: annotation for abstract values

data Ann 
  = Keyword               -- ^ Keywords, e.g., @let@ or @assume@.
  | Identifier IdentKind  -- ^ Identifiers, like variables and types.
  | Literal LitKind       -- ^ Literals, e.g., integers or strings.
  | Bracket BraKind       -- ^ Matched nesting symbols, e.g., parentheses.
  | Separator             -- ^ Syntactic separators, e.g., commas.
  | Comment               -- ^ Comments.
  | Highlight             -- ^ Highlighted piece of syntax, something notable.
  | Message               -- ^ Any kind of message from the compiler.
  | Error                 -- ^ Something erroneous.
  | Success               -- ^ Something successful.
  | Margin                -- ^ Marginalia, like line numbers and such.
  | ASCII Text            -- ^ Alternative ASCII version of a Unicode symbol.
  | NormalWeight          -- ^ Prevent bold styling from being applied.
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
