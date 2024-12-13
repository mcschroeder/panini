{-# OPTIONS_GHC -Wno-orphans #-}  -- TODO: remove
module Panini.Diagnostic 
  ( Diagnostic(..)  
  , DiagnosticEnvelope(..)
  , Severity(..)
  , isError
  , prettyError
  , prettyErrorDiagnostic
  ) where

import Data.Text (Text)
import Data.Text qualified as Text
import Panini.Pretty
import Panini.Provenance
import Prelude
import Prettyprinter qualified as PP
import Control.Exception

-------------------------------------------------------------------------------

class Diagnostic a where
  diagnosticMessage :: a -> Doc

instance Diagnostic Doc where
  diagnosticMessage = id

instance Diagnostic IOError where  
  diagnosticMessage e = pretty $ displayException e

-------------------------------------------------------------------------------

data DiagnosticEnvelope a = DiagnosticEnvelope
  { rapporteur :: String
  , severity :: Severity
  , provenance :: PV
  , diagnostic :: a
  } 
  deriving stock (Functor, Foldable, Traversable)

data Severity = SevError | SevWarning | SevInfo | SevTrace
  deriving stock (Eq, Ord, Show)

isError :: DiagnosticEnvelope a -> Bool
isError = (SevError ==) . severity

instance HasProvenance (DiagnosticEnvelope a) where
  getPV = provenance
  setPV pv e = e { provenance = pv }

-------------------------------------------------------------------------------

-- TODO: where to put this?

prettyError :: (Diagnostic a, HasProvenance a) => a -> Doc
prettyError e = prettyErrorDiagnostic (diagnosticMessage e) (getPV e)

prettyErrorDiagnostic :: Doc -> PV -> Doc
prettyErrorDiagnostic msg pv = nest 2 $ ann Message (header <+> group msg) <> source
 where
  (loc,src) = prettyLoc pv
  header    = loc <> ":" <+> ann Error "error" <> ":"
  source    = (maybe mempty (PP.hardline <>) src)

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
