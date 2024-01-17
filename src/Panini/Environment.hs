{-# LANGUAGE RecordWildCards #-}
module Panini.Environment where

import Data.Function
import Data.Generics.Uniplate.Operations
import Data.List qualified as List
import Data.Map (Map)
import Data.Map qualified as Map
import Panini.Abstract.AExpr
import Panini.Abstract.AString (AString)
import Panini.Panic
import Panini.Pretty
import Panini.Provenance
import Panini.Solver.Assignment
import Panini.Solver.Constraints
import Panini.Syntax
import Prelude

-------------------------------------------------------------------------------

type Environment = Map Name Definition

data Definition
    = Assumed
        { _name :: Name
        , _type :: Type
        }
    | Verified
        { _name         :: Name
        , _assumedType  :: Maybe Type
        , _term         :: Term
        , _inferredType :: Type
        , _vc           :: Con
        , _solution     :: Assignment
        , _solvedType   :: Type
        }

    deriving stock (Show, Read)

-------------------------------------------------------------------------------

isVerified :: Definition -> Bool
isVerified = \case
  Assumed{} -> False
  Verified{} -> True

-------------------------------------------------------------------------------

data TypeSig = TypeSig Name Type
  deriving stock (Eq, Show, Read)

instance Pretty TypeSig where
  pretty (TypeSig x t) = pretty x <+> ":" <+> pretty t

toTypeSig :: Definition -> TypeSig
toTypeSig = \case
  Assumed{_name,_type} -> TypeSig _name _type
  Verified{_name,_solvedType} -> TypeSig _name _solvedType

-------------------------------------------------------------------------------

-- | Return type signatures for all verified definitions in the environment.
getVerifiedTypes :: Environment -> [TypeSig]
getVerifiedTypes = map (toTypeSig . snd)
                 . List.sortBy (compare `on` getPV . fst) 
                 . Map.toList 
                 . Map.filter isVerified

-- | Return all verified grammars in the environment.
getVerifiedGrammars :: Environment -> [AString]
getVerifiedGrammars = concatMap extractGrammars 
                    . map (\(TypeSig _ t) -> t)
                    . getVerifiedTypes
  where    
    -- TODO: this is pretty hacky and limited
    extractGrammars :: Type -> [AString]
    extractGrammars (TFun _ t1 t2 _) = extractGrammars t1 ++ extractGrammars t2
    extractGrammars t@(TBase x TString (Known p) _) = case p of
      PRel (EVar y :∈: EStrA s) | x == y -> [s]
      _ | not $ null [True | PRel (_ :∈: _) <- universe p ] -> 
          panic $ "extractGrammars: irregular grammar:" <+> pretty t
      _ -> []
    extractGrammars _ = []
