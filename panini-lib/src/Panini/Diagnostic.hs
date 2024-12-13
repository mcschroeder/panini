{-# OPTIONS_GHC -Wno-orphans #-}  -- TODO: remove
module Panini.Diagnostic (Diagnostic(..), prettyDiagnostic) where

import Control.Exception (displayException)
import Data.Text (Text)
import Data.Text qualified as Text
import Panini.Pretty
import Panini.Provenance
import Prelude
import Prettyprinter qualified as PP

-------------------------------------------------------------------------------

-- TODO: remove HasProvenance constraint

class HasProvenance a => Diagnostic a where
  diagnosticMessage :: a -> Doc

instance Diagnostic IOError where
  diagnosticMessage e = pretty $ displayException e

-- TODO: remove instance
instance HasProvenance IOError where
  getPV _ = NoPV
  setPV _ = id

-------------------------------------------------------------------------------

prettyDiagnostic :: Diagnostic a => a -> Doc
prettyDiagnostic diag = nest 2 $ ann Message (header <+> group message) <> source
 where
  (loc,src) = prettyLoc $ getPV diag
  header    = loc <> ":" <+> ann Error "error" <> ":"
  source    = (maybe mempty (PP.hardline <>) src)
  message   = diagnosticMessage diag

prettyLoc :: PV -> (Doc, Maybe Doc)
prettyLoc = \case
  FromSource l (Just s) -> (pretty l, Just (wavyDiagnostic l s))
  FromSource l Nothing  -> (pretty l, Nothing)
  Derived pv _          -> prettyLoc pv
  NoPV                  -> ("<unknown location>", Nothing)

wavyDiagnostic :: SrcLoc -> Text -> Doc
wavyDiagnostic (SrcLoc _ (l1,c1) (l2,c2)) s =
  ann Margin (mPadding   <+> "│") <\\>
  ann Margin (lineNumber <+> "│") <+> offendingLine <\\>
  ann Margin (mPadding   <+> "│") <+> errorPointer
  where
    mPadding       = pretty $ replicate (length (show l1)) ' '
    lineNumber     = pretty $ show l1
    offendingLine  = pretty lineL <> ann Error (pretty lineE) <> pretty lineR
    errorPointer   = pretty pPadding <> ann Error (pretty pointer)
    (lineL, s')    = Text.splitAt (c1 - 1) s
    (lineE, lineR) = Text.splitAt eLen s'
    pointer        = replicate pLen '^'
    pPadding       = if pLen > 0 then replicate pShift ' ' else ""
    pLen           = if pShift + eLen > sLen then sLen - pShift + 1 else eLen
    pShift         = c1 - 1
    eLen           = if l1 == l2 then c2 - c1 else 1
    sLen           = Text.length s
