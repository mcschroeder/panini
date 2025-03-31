module Panini.Solver.Error where

import Panini.Abstract.AValue
import Panini.Diagnostic
import Panini.Pretty
import Panini.Provenance
import Panini.SMT.Event qualified as SMT
import Panini.Syntax
import Prelude

data Error where
  AbstractionToValueImpossible  :: Name -> ARel -> AValue -> Error
  SmtEvent                      :: SMT.Event -> Error

instance Diagnostic Error where
  diagnosticMessage = \case
    AbstractionToValueImpossible x r e ->
      "abstraction to value impossible:" <\> 
      "⟦" <> pretty r <> "⟧↑" <> pretty x <+> "≐" <+> pretty e
    
    SmtEvent e -> diagnosticMessage e

instance HasProvenance Error where  
  getPV = \case
    AbstractionToValueImpossible x _ _ -> getPV x -- TODO
    SmtEvent _ -> NoPV
