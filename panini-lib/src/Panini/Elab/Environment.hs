{-# LANGUAGE RecordWildCards #-}
module Panini.Elab.Environment where

import Data.Function
import Data.List qualified as List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe
import Panini.Elab.Definition
import Panini.Elab.Error
import Panini.Pretty
import Panini.Provenance
import Panini.Syntax
import Prelude

------------------------------------------------------------------------------

-- | The elaborator environment stores definitions, i.e., mappings from names
-- (of variables, constants, functions) to their types. During elaboration, the
-- state of these types might change, e.g., an inferred type might be found to
-- have an invalid verification condition during SMT solving (see 'Definition').
type Environment = Map Name Definition

-------------------------------------------------------------------------------

-- | All definitions in the environment, sorted by order of appearance
-- (according to provenance information).
sortedDefinitions :: Environment -> [Definition]
sortedDefinitions = 
  map snd . List.sortBy (compare `on` getPV . fst) . Map.toList

-- | Returns all type errors in the environment.
getTypeErrors :: Environment -> [ElabError]
getTypeErrors = map _error . filter isFailed . sortedDefinitions

-- | Return type signatures for all fully solved definitions in the environment,
-- both verified and unverified.
getSolvedTypes :: Environment -> [TypeSig]
getSolvedTypes = catMaybes . map go . sortedDefinitions
 where
  go Verified{..}   = Just $ TypeSig _name _solvedType Nothing
  go Unverified{..} = Just $ TypeSig _name _solvedType (Just comment)
                        where comment = "UNVERIFIED (" ++ _reason ++ ")"
  go _ = Nothing

-- | A type signature, with an optional comment.
data TypeSig = TypeSig Name Type (Maybe String)
  deriving stock (Eq, Show, Read)

instance Pretty TypeSig where
  pretty (TypeSig x t c) = pretty x <+> ":" <+> pretty t <> comment
   where
    comment | Just m <- c = "  " <> ann Comment ("--" <+> pretty m)
            | otherwise   = mempty
