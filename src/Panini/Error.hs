module Panini.Error where

import Data.Text (Text)
import Data.Text qualified as Text
import Panini.Abstract.AValue
import Panini.Frontend.Python.Error qualified as Python
import Panini.Pretty
import Panini.Provenance
import Panini.Solver.Constraints
import Panini.Syntax
import Prelude
import Prettyprinter qualified as PP

-------------------------------------------------------------------------------

data Error
  = AlreadyDefined Name
  | UnknownVar Name
  | InvalidSubtype Type Type
  | ExpectedFunType Term Type
  | ParserError Text PV
  | SolverError Text PV
  | Unsolvable Name Con
  | IOError String PV
  | AbstractionImpossible Name Rel Rel
  | ConcretizationImpossible Name Base AValue
  | PythonFrontendError Python.Error PV

instance HasProvenance Error where
  getPV = \case
    AlreadyDefined x               -> getPV x
    UnknownVar x                   -> getPV x
    InvalidSubtype t _             -> getPV t
    ExpectedFunType e _            -> getPV e
    ParserError _ pv               -> pv
    SolverError _ pv               -> pv
    Unsolvable x _                 -> getPV x
    IOError _ pv                   -> pv
    AbstractionImpossible x _r1 _  -> getPV x -- TODO: getPV r1
    ConcretizationImpossible x _ _ -> getPV x -- TODO: getPV a
    PythonFrontendError _ pv       -> pv
  
  setPV pv = \case
    AlreadyDefined x               -> AlreadyDefined (setPV pv x)
    UnknownVar x                   -> UnknownVar (setPV pv x)
    InvalidSubtype t1 t2           -> InvalidSubtype (setPV pv t1) t2
    ExpectedFunType e t            -> ExpectedFunType (setPV pv e) t
    ParserError e _                -> ParserError e pv
    SolverError e _                -> SolverError e pv
    Unsolvable x vc                -> Unsolvable (setPV pv x) vc
    IOError e _                    -> IOError e pv
    AbstractionImpossible x r1 r2  -> AbstractionImpossible (setPV pv x) r1 r2  -- TODO: setPV r1
    ConcretizationImpossible x b a -> ConcretizationImpossible (setPV pv x) b a -- TODO: setPV a
    PythonFrontendError e _        -> PythonFrontendError e pv

-------------------------------------------------------------------------------

instance Pretty Error where
  pretty = prettyError

prettyError :: Error -> Doc
prettyError err = nest 2 $ ann Message (header <+> group message) <> source
 where
  (loc,src) = prettyLoc $ getPV err
  header    = loc <> ":" <+> ann Error "error" <> ":"
  source    = (maybe mempty (PP.hardline <>) src)
  message   = prettyErrorMessage err

prettyErrorMessage :: Error -> Doc
prettyErrorMessage = \case
  AlreadyDefined x     -> "multiple definitions for" <\> pretty x    
  UnknownVar x         -> "unknown variable" <\> pretty x        
  InvalidSubtype t1 t2 -> "invalid subtype:" <\> pretty t1 <+> "<:" <+> pretty t2
  ExpectedFunType _ t  -> "invalid function type:" <\> pretty t
  ParserError e _      -> pretty $ Text.stripEnd e
  SolverError e _      -> "unexpected SMT solver output:" <\> pretty e
  Unsolvable x _       -> "cannot solve constraints of" <\> pretty x    
  IOError e _          -> pretty $ Text.pack e  
  AbstractionImpossible x _ r2 -> 
    "abstraction impossible:" <\> "⟦" <> pretty r2 <> "⟧↑" <> pretty x
  ConcretizationImpossible x b a ->
    "concretization impossible for" <+> pretty b <> ":" <\> 
    "⟦" <> pretty a <> "⟧↓" <> pretty x
  PythonFrontendError e _ -> pretty e

prettyLoc :: PV -> (Doc, Maybe Doc)
prettyLoc (FromSource l (Just s)) = (pretty l, Just (wavyDiagnostic l s))
prettyLoc (FromSource l Nothing) = (pretty l, Nothing)
prettyLoc (Derived pv _) = prettyLoc pv
prettyLoc NoPV = ("<unknown location>", Nothing)

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
