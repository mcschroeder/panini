{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedLists #-}

module Panini.Elab
  ( elaborate
  , assume
  , define
  , import_
  ) where

import Control.Monad.Extra
import Control.Monad.Trans.State.Strict
import Data.Map qualified as Map
import Data.Maybe
import Data.Set qualified as Set
import Data.Text.IO qualified as Text
import Panini.Elab.Definition
import Panini.Elab.Error
import Panini.Elab.Module
import Panini.Infer
import Panini.Monad
import Panini.Panic
import Panini.Parser (parseProgram)
import Panini.Pretty
import Panini.Provenance
import Panini.Solver qualified as Solver
import Panini.Solver.Assignment
import Panini.Solver.Simplifier
import Panini.Syntax
import Prelude
import System.Directory
import System.FilePath

-------------------------------------------------------------------------------

-- | Retrieve a definition from the environment.
envLookup :: Name -> Pan e (Maybe Definition)
envLookup x = Map.lookup x <$> gets environment

-- | Extend the environment with a new definition.
envExtend :: Name -> Definition -> Pan e ()
envExtend x d = do
  -- TODO: what was this even?
  -- when (isFailed d) $ logError d._error
  modify' $ \s -> s { environment = Map.insert x d s.environment }

-------------------------------------------------------------------------------

-- | Elaborate a program by elaborating all of its statments and updating the
-- environment accordingly. If an unrecoverable error occurs, any intermittent
-- changes to the environment are rolled back. Note that errors during type
-- inference or VC solving usually merely result in a 'Rejected' or 'Invalid'
-- definition to be stored in the environment and will *not* cause any rollback.
elaborate :: Module -> Pan ElabError ()
elaborate thisModule = do
  env0 <- get
  when (thisModule `elem` env0.loadedModules) $ do
    panic "reloading modules is not yet implemented" -- TODO
  tryError (mapM_ elab thisModule.program) >>= \case
    Right () -> do
      modify' $ \s -> s { loadedModules = thisModule : s.loadedModules }
    Left err -> do
      put env0
      throwError err
 where
  elab = \case
    Assume x t  -> assume x t
    Define x e  -> define x e
    Import f pv -> import_ thisModule f pv

-- | Add an assumed type to the environment.
assume :: Name -> Type -> Pan ElabError ()
assume x t = do
  info $ "Assume" <+> pretty x <+> align (":" <+> pretty t)
  whenJustM (envLookup x) $ \_ -> throwError $ AlreadyDefined x
  envExtend x (Assumed x t)

-- | Add a definition to the environment. Infers and verifies the definition's
-- type and potentially reconciles it with a previously assumed type for the
-- same name.
define :: Name -> Term -> Pan ElabError ()
define x e = do
  info $ "Define" <+> pretty x
  t0m <- envLookup x >>= \case
    Nothing                -> return Nothing
    Just (Assumed {_type}) -> return $ Just _type
    Just _                 -> throwError $ AlreadyDefined x

  info $ "Infer type of" <+> pretty x
  let syn = maybe (infer mempty e) (check mempty e) t0m
  (tryError syn ?? TypeError) >>= \case
    Left err -> do
      envExtend x $ Rejected x t0m e (TypeError err)

    Right (t1,vc) -> do
      trace $ pretty t1
      t2 <- simplify t1 § "Simplify type"      
      envExtend x $ Inferred x t0m e t2 vc
            
      -- TODO: less brittle way of identifying user holes
      -- TODO: move into solver; this is pretty much just for Fusion
      let isUserHole k | Derived _ "shape" <- getPV k = False
                       | otherwise                    = True
      ks_ex <- Set.filter isUserHole (kvars vc) § "Find user-defined type holes"
      --ks_ex <- kvars t2 § "Find top-level type holes"

      info @Doc "Solve VC"
      trace $ pretty vc
      (tryError (Solver.solve ks_ex vc) ?? SolverError) >>= \case
        Left err -> do
          envExtend x $ Invalid x t0m e t2 vc (SolverError err)

        Right Solver.Invalid -> do
          envExtend x $ Invalid x t0m e t2 vc (Unsolvable x vc)
        
        Right (Solver.Unverified s r) -> do
          t3 <- makeFinalType s t2 t0m
          envExtend x $ Unverified x t0m e t2 vc s t3 r

        Right (Solver.Valid s) -> do
          t3 <- makeFinalType s t2 t0m
          envExtend x $ Verified x t0m e t2 vc s t3

makeFinalType :: Assignment -> Type -> Maybe Type -> Pan ElabError Type
makeFinalType s t1 t0m = do
  t2 <- apply s t1        § "Apply solution to type"
  t3 <- simplify t2       § "Simplify type"
  case t0m of
    Nothing -> pure t3
    Just t0 -> do
      info @Doc "Match inferred type against signature"
      let (t4,w) = matchTypeSig t3 t0
      trace $ pretty t4
      when w $ info $ warning t0 t3 t4
      return t4
 where
  -- TODO: attach proper warning type to definition in environment
  warning user inferred final = nest 2 $ "Warning:" <\>
    hang 2 ("User-provided type annotation" <\> pretty user) <\>
    hang 2 ("overrides more precise inferred type" <\> pretty inferred) <\>
    hang 2 ("resulting in less precise final type" <\> pretty final)

-- TODO: rename inferred vars to match provided sig?
-- | Match an inferred type signature against a user-provided annotation.
matchTypeSig :: Type -> Type -> (Type, Bool)
matchTypeSig = go
 where
  go (TBase v1 b1 r1 pv1) (TBase v2 b2 r2 pv2) 
    | b1 == b2 = case (r1,r2) of
      (Unknown , _       ) -> impossible
      (Known p1, Known p2) -> (TBase v2 b2 r2 pv2, p1 /= p2)
      (Known p1, Unknown ) -> (TBase v2 b1 r  pv1, False) 
        where 
          r = Known $ subst (EVar v2 b2) v1 p1
  
  go (TFun x1 s1 t1 pv1) (TFun x2 s2 t2 _) = 
    (TFun x2 s t pv1, ws || wt)
    where 
      (s, ws) = go s1  s2
      (t, wt) = go t1' t2
      t1'     = subst (EVar x2 b2) x1 t1
      b2      = case s2 of
                  TBase _ b _ _ -> b
                  TFun  _ _ _ _ -> TUnit  -- TODO

  go _ _ = impossible

-- TODO: attach provenance from import statement to any errors
-- | Resolve an import relative to the current module.
import_ :: Module -> FilePath -> PV -> Pan ElabError ()
import_ thisModule relPath _ = do
  info $ "Import" <+> pretty relPath
  parentDir <- case thisModule.moduleOrigin of
    File  f -> return $ normalise $ takeDirectory f
    Stdin _ -> tryIO (makeAbsolute =<< getCurrentDirectory) ?? IOError
    REPL  _ -> tryIO (makeAbsolute =<< getCurrentDirectory) ?? IOError
  let path = parentDir </> relPath
  redundant <- (elem (File path) . map moduleOrigin) <$> gets loadedModules
  unless redundant $ do
    src <- (tryIO $ Text.readFile path) ?? IOError   §§ "Read" <+> pretty path
    importProg <- parseProgram path src ? ParseError §§ "Parse source"
    elaborate Module 
      { moduleOrigin = File path
      , sourceType   = "panini"
      , program      = importProg 
      }
