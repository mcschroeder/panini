module Panini.Error where

import Data.List qualified as List
import Data.Text (Text)
import Data.Text qualified as Text
import Panini.Pretty
import Panini.Provenance
import Panini.Solver.Constraints
import Panini.Syntax
import Prelude

-------------------------------------------------------------------------------

data Error
  = AlreadyDefined Name
  | VarNotInScope Name
  | MissingType Name
  | InvalidSubtypeBase (Type,Base) (Type,Base)  -- TODO: do we need this?
  | InvalidSubtype Type Type
  | ExpectedFunType Term Type
  | CantSynth Term
  | ParserError PV Text
  | SolverError Text
  | InvalidVC Name Con
  | IOError PV String
  | AbstractionImpossible Rel Name
  | ConcretizationImpossible Expr Name
  deriving stock (Show, Read)

instance HasProvenance Error where
  getPV (AlreadyDefined x) = getPV x
  getPV (VarNotInScope x) = getPV x
  getPV (MissingType x) = getPV x
  getPV (InvalidSubtypeBase (t,_) _) = getPV t
  getPV (InvalidSubtype t _) = getPV t
  getPV (ExpectedFunType _e _) = NoPV --getPV e
  getPV (CantSynth _e) = NoPV --getPV e
  getPV (ParserError pv _) = pv
  getPV (SolverError _) = NoPV
  getPV (InvalidVC x _) = getPV x
  getPV (IOError pv _) = pv
  getPV (AbstractionImpossible _ x) = getPV x -- TODO: ?
  getPV (ConcretizationImpossible _ x) = getPV x -- TODO: ?

  setPV pv (AlreadyDefined x) = AlreadyDefined (setPV pv x)
  setPV pv (VarNotInScope x) = VarNotInScope (setPV pv x)
  setPV pv (MissingType x) = MissingType (setPV pv x)
  setPV pv (InvalidSubtypeBase (t1,b1) y) = InvalidSubtypeBase (setPV pv t1, b1) y
  setPV pv (InvalidSubtype t1 t2) = InvalidSubtype (setPV pv t1) t2
  setPV _ e@(ExpectedFunType _e _) = e -- TODO
  setPV _ e@(CantSynth _e) = e -- TODO
  setPV pv (ParserError _ e) = ParserError pv e
  setPV _ e@(SolverError _) = e
  setPV pv (InvalidVC x vc) = InvalidVC (setPV pv x) vc
  setPV pv (IOError _ e) = IOError pv e
  setPV pv (AbstractionImpossible r x) = AbstractionImpossible r (setPV pv x)
  setPV pv (ConcretizationImpossible e x) = ConcretizationImpossible e (setPV pv x)

-------------------------------------------------------------------------------

instance Pretty Error where
  pretty e = case prettyLoc $ getPV e of
    (loc, Just src) -> message loc <\\> src
    (loc, Nothing ) -> message loc
    where
      message o  = nest 2 (header o <\\> reason)
      header o   = ann Message (o <> ":" <+> ann Error "error:")
      reason     = prettyErrorMessage e

prettyLoc :: PV -> (Doc, Maybe Doc)
prettyLoc (FromSource l (Just s)) = (pretty l, Just (wavyDiagnostic l s))
prettyLoc (FromSource l Nothing) = (pretty l, Nothing)
prettyLoc (Derived pv _) = prettyLoc pv
prettyLoc NoPV = ("<unknown location>", Nothing)

prettyErrorMessage :: Error -> Doc
prettyErrorMessage = \case
  AlreadyDefined x -> pretty x <+> msg "is already defined"    
  VarNotInScope n -> msg "Variable not in scope:" <+> pretty n  
  MissingType n -> msg "Missing type definition for" <+> pretty n  
  
  InvalidSubtypeBase (t1,b1) (t2,b2) -> bullets
    [ pretty b1 <+> msg "is not a subtype of" <+> pretty b2
    , group $ nest 4 (msg "Therefore," <\> pretty t1) <\> 
      nest 4 (msg "is not a subtype of" <\> pretty t2)
    ]  
  
  InvalidSubtype t1 t2 -> 
    pretty t1 <+> msg "is not a subtype of" <+> pretty t2  
  
  ExpectedFunType e t -> bullets
    [ pretty t <+> msg "is not a function type"
    , group $ nest 4 $ msg "Expected a function type for expression:" <\> 
      pretty e
    ]

  CantSynth e -> 
    group $ nest 4 $ msg "Can't synthesize type for expression:" <\> pretty e

  ParserError _ e -> msg $ Text.stripEnd e

  SolverError e -> bullets
    [ msg "The SMT solver returned some unexpected output:" <\> pretty e ]
  
  InvalidVC x _ -> bullets
    [ msg "Unable to validate verification condition for" <+> pretty x ]

  IOError _ e -> msg $ Text.pack e

  AbstractionImpossible r x -> bullets
    [ group $ nest 4 $ msg "Abstraction impossible:" <\> 
        "⟦" <> pretty r <> "⟧↑" <> pretty x
    ]
  
  ConcretizationImpossible e x -> bullets
    [ group $ nest 4 $ msg "Concretization impossible:" <\> 
        "⟦" <> pretty e <> "⟧↓" <> pretty x 
    ]

  where
    msg     = ann Message . pretty @Text
    bullets = mconcat . List.intersperse "\n" . map ("•" <+>)

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
