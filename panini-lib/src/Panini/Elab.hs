{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedLists #-}

module Panini.Elab
  ( elaborate
  , assume
  , define
  , import_
  , parseSource
  ) where

import Control.Monad.Extra
import Control.Monad.Trans.State.Strict
import Data.Map qualified as Map
import Data.Maybe
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text.IO qualified as Text
import Panini.Environment
import Panini.Error
import Panini.Infer
import Panini.Modules
import Panini.Monad
import Panini.Panic
import Panini.Parser qualified
import Panini.Pretty
import Panini.Provenance
import Panini.Solver.Assignment
import Panini.Solver qualified as Solver
import Panini.Solver.Simplifier
import Panini.Syntax
import Prelude

-------------------------------------------------------------------------------

-- | Retrieve a definition from the environment.
envLookup :: Name -> Pan Error (Maybe Definition)
envLookup x = Map.lookup x <$> gets environment

-- | Extend the environment with a new definition.
envExtend :: Name -> Definition -> Pan Error ()
envExtend x d = do
  when (isFailed d) $ logError d._error
  modify' $ \s -> s { environment = Map.insert x d s.environment }

-- TODO: do we need this?
-- -- | Remove a definition from the environment.
-- envDelete :: Name -> Pan Error ()
-- envDelete x = modify' $ \s -> s { environment = Map.delete x s.environment }

-------------------------------------------------------------------------------

-- | Elaborate a program by elaborating all of its statments and updating the
-- environment accordingly. If an unrecoverable error occurs, any intermittent
-- changes to the environment are rolled back. Note that errors during type
-- inference or VC solving usually merely result in a 'Rejected' or 'Invalid'
-- definition to be stored in the environment and will *not* cause any rollback.
elaborate :: Module -> Program -> Pan Error ()
elaborate thisModule prog = do
  env0 <- get
  unless (thisModule == replModule) $
    when (thisModule `elem` env0.loadedModules) $ do
      panic "reloading modules is not yet implemented" -- TODO
  tryError (mapM_ elab prog) >>= \case
    Right () -> do
      unless (thisModule == replModule) $
        modify' $ \s -> s { loadedModules = thisModule : s.loadedModules }
    Left err -> do
      put env0
      throwError err
 where
  elab = \case
    Assume x t  -> assume x t
    Define x e  -> define x e
    Import fp _ -> import_ $ moduleRelativeTo thisModule fp
                      -- TODO: add provenance to error

-- | Add an assumed type to the environment.
assume :: Name -> Type -> Pan Error ()
assume x t = do
  logMessage $ "Assume" <+> pretty x <+> ":" <+> pretty t
  whenJustM (envLookup x) $ \_ -> throwError $ AlreadyDefined x
  envExtend x (Assumed x t)

-- | Add a definition to the environment. Infers and verifies the definition's
-- type and potentially reconciles it with a previously assumed type for the
-- same name.
define :: Name -> Term -> Pan Error ()
define x e = do
  logMessage $ "Define" <+> pretty x <+> "= ..."
  t0m <- envLookup x >>= \case
    Nothing                -> return Nothing
    Just (Assumed {_type}) -> return $ Just _type
    Just _                 -> throwError $ AlreadyDefined x

  logMessage $ "Infer type of" <+> pretty x
  let syn = maybe (infer mempty e) (check mempty e) t0m
  tryError syn >>= \case
    Left err -> do
      envExtend x $ Rejected x t0m e err

    Right (t1,vc) -> do
      logData t1
      t2 <- simplify t1 § "Simplify type"      
      envExtend x $ Inferred x t0m e t2 vc
            
      -- TODO: less brittle way of identifying user holes
      -- TODO: move into solver; this is pretty much just for Fusion
      let isUserHole k | Derived _ "shape" <- getPV k = False
                       | otherwise                    = True
      ks_ex <- Set.filter isUserHole (kvars vc) § "Find user-defined type holes"
      --ks_ex <- kvars t2 § "Find top-level type holes"

      logMessage "Solve VC"
      logData vc
      tryError (Solver.solve ks_ex vc) >>= \case
        Left err -> do
          envExtend x $ Invalid x t0m e t2 vc err

        Right Solver.Invalid -> do
          envExtend x $ Invalid x t0m e t2 vc (Unsolvable x vc)
        
        Right (Solver.Unverified s r) -> do
          t3 <- makeFinalType s t2 t0m
          envExtend x $ Unverified x t0m e t2 vc s t3 r

        Right (Solver.Valid s) -> do
          t3 <- makeFinalType s t2 t0m
          envExtend x $ Verified x t0m e t2 vc s t3

makeFinalType :: Assignment -> Type -> Maybe Type -> Pan Error Type
makeFinalType s t1 t0m = do
  t2 <- apply s t1        § "Apply solution to type"
  t3 <- simplify t2       § "Simplify type"
  case t0m of
    Nothing -> pure t3
    Just t0 -> do
      logMessage "Match inferred type against signature"
      let (t4,w) = matchTypeSig t3 t0
      logData t4
      when w $ logMessage $ warning t0 t3 t4
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
          r = Known $ subst (EVar v2) v1 p1
  
  go (TFun x1 s1 t1 pv1) (TFun x2 s2 t2 _) = 
    (TFun x2 s t pv1, ws || wt)
    where 
      (s, ws) = go s1  s2
      (t, wt) = go t1' t2
      t1'     = subst (EVar x2) x1 t1

  go _ _ = impossible

-- | Import a module into the environment.
import_ :: Module -> Pan Error ()
import_ otherModule = do
  logMessage $ "Import" <+> pretty otherModule
  redundant <- elem otherModule <$> gets loadedModules
  unless redundant $ do
    otherSrc <- (tryIO $ Text.readFile $ moduleLocation otherModule) ?? IOError
    otherProg <- parseSource (moduleLocation otherModule) otherSrc ?? ParseError
    elaborate otherModule otherProg

-- TODO: maybe not the right location for this
parseSource :: FilePath -> Text -> Pan Panini.Parser.Error Program
parseSource path src = do
  logMessage $ "Parse" <+> pretty path
  prog <- Panini.Parser.parseProgram path src ? id
  logData prog
  return prog
