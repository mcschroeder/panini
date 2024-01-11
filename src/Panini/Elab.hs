{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedLists #-}

module Panini.Elab
  ( elaborate
  , assume
  , define
  , import_
  , envToContext -- TODO: weird place for this?
  ) where

import Control.Monad.Extra
import Control.Monad.Trans.State.Strict
import Data.Map qualified as Map
import Data.Maybe
import Data.Text.IO qualified as Text
import Panini.Environment
import Panini.Error
import Panini.Infer
import Panini.Modules
import Panini.Monad
import Panini.Panic
import Panini.Parser
import Panini.Pretty
import Panini.Provenance
import Panini.Solver
import Panini.Syntax
import Prelude

-------------------------------------------------------------------------------

-- | Retrieve a definition from the environment.
envLookup :: Name -> Pan (Maybe Definition)
envLookup x = Map.lookup x <$> gets environment

-- | Extend the environment with a new definition.
envExtend :: Name -> Definition -> Pan ()
envExtend x d = modify' $ \s -> s 
  { environment = Map.insert x d s.environment
  }

-- TODO: do we need this?
-- -- | Remove a definition from the environment.
-- envDelete :: Name -> Pan ()
-- envDelete x = modify' $ \s -> s { environment = Map.delete x s.environment }

-- | Convert an elaborator environment to a typechecking context by throwing
-- away all non-final definitions.
envToContext :: Environment -> Context 
envToContext = Map.map go
  where
    go (Assumed {_type}) = _type
    go (Verified {_solvedType}) = _solvedType

-------------------------------------------------------------------------------

-- | Elaborate a program by elaborating all of its statments and updating the
-- environment accordingly. If elaboration fails at any point, an error is
-- thrown and any intermittent changes to the environment are rolled back.
elaborate :: Module -> Program -> Pan ()
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
assume :: Name -> Type -> Pan ()
assume x t = do
  logMessage $ "Assume" <+> pretty x <+> ":" <+> pretty t
  whenJustM (envLookup x) $ \_ -> throwError $ AlreadyDefined x -- TODO: AlreadyAssumed
  envExtend x (Assumed x t)

-- | Add a definition to the environment. Infers and verifies the definition's
-- type and potentially reconciles it with a previously assumed type for the
-- same name.
define :: Name -> Term -> Pan ()
define x e = do
  logMessage $ "Define" <+> pretty x <+> "= ..."
  t0m <- envLookup x >>= \case
    Nothing                -> return Nothing
    Just (Assumed {_type}) -> return $ Just _type
    Just (Verified {})     -> throwError $ AlreadyDefined x

  logMessage "Prepare typing context Γ"
  g <- envToContext <$> gets environment
  logData g

  logMessage $ "Infer type of" <+> pretty x
  (t1,vc) <- case t0m of
    Nothing -> infer g e
    Just t0 -> infer g $ Rec x t0 e (Val (Var x)) NoPV  
  logData t1

  let ks_ex = kvars t1
  logMessage $ "Top-level type holes:" <+> pretty ks_ex

  logMessage "Solve VC"
  logData vc
  solve ks_ex vc >>= \case
    Just s -> do
      logMessage "Apply solution to type"
      let t2 = apply s t1
      logData t2
      
      logMessage $ "Match inferred type against" <+> pretty t0m
      t3 <- case t0m of
        Nothing -> return t2
        Just t̃ -> matchTypeSig t2 t̃
      logData t3
      
      envExtend x $ Verified x t0m e t1 vc s t3
    
    Nothing -> do
      throwError $ InvalidVC x vc

-- TODO: rename inferred vars to match provided sig?
-- | Match an inferred type signature against a user-provided annotation.
matchTypeSig :: Type -> Type -> Pan Type
matchTypeSig inferred user = do
  let (t,w) = go inferred user
  when w $ logMessage $ nest 2 $ "Warning:" <\>
    hang 2 ("User-provided type annotation" <\> pretty user) <\>
    hang 2 ("overrides more precise inferred type" <\> pretty inferred) <\>
    hang 2 ("resulting in less precise final type" <\> pretty t)
  return t
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
import_ :: Module -> Pan ()
import_ otherModule = do
  logMessage $ "Import" <+> pretty otherModule
  redundant <- elem otherModule <$> gets loadedModules
  unless redundant $ do
    otherSrc <- tryIO NoPV $ Text.readFile $ moduleLocation otherModule
    otherProg <- parseSource (moduleLocation otherModule) otherSrc
    elaborate otherModule otherProg
