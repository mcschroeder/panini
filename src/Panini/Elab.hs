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
import Data.Set (Set)
import Data.Set qualified as Set
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
  (t1,vc) <- infer g e
  logData t1  

  logMessage $ "Match inferred type against assumption:" <+> pretty t0m
  (s0, ks_ex) <- fromMaybe (mempty, kvars t1) <$> mapM (matchTypeAnnotation t1) t0m
  logMessage $ "Top-level type holes:" <+> pretty ks_ex
  logMessage $ "Top-level assignment:" <+> pretty s0

  logMessage "Solve VC"
  logData vc
  solve s0 ks_ex vc >>= \case
    Just s -> do
      logMessage "Apply solution to type"
      let t2 = apply s t1
      logData t2
      envExtend x $ Verified x t0m e t1 vc s t2
    
    Nothing -> do
      throwError $ InvalidVC x vc

-- | Matches an inferred type signature against a user-provided annotation,
-- extracting top-level κ assignments and collecting those κs that stand for
-- explicit type holes.
matchTypeAnnotation :: Type -> Type -> Pan (Assignment, Set KVar)
matchTypeAnnotation inferred user = do
  s0 <- go [] inferred user
  let s_top   = Map.fromList [(k,p) | (k, Known p) <- Map.toList s0]
  let k_holes = Set.fromList [ k    | (k, Unknown) <- Map.toList s0]
  return (s_top, k_holes)
 where
  go g (TBase x1 b1 (Known (PAppK k ys)) _) (TBase x2 b2 r2 _)
    | b1 == b2 = do
      let kps = Map.fromList $ zip [y | EVar y <- ys] (kparams k)
      assertM $ Map.size kps == length ys
      let xs = [ (EVar x_user, k_param) 
               | (x_user, x_inf) <- Map.toList $ Map.insert x2 x1 g
               , Just k_param <- [Map.lookup x_inf kps]
               ]
      let r2' = uncurry substN (unzip xs) r2
      return $ Map.singleton k r2'
  
  go g (TFun x1 s1 t1 _) (TFun x2 s2 t2 _) = do
    a1 <- go g s1 s2
    a2 <- go (Map.insert x2 x1 g) t1 t2
    return $ a1 <> a2
  
  go _ _ _ = throwError $ InvalidSubtype inferred user


-- | Import a module into the environment.
import_ :: Module -> Pan ()
import_ otherModule = do
  logMessage $ "Import" <+> pretty otherModule
  redundant <- elem otherModule <$> gets loadedModules
  unless redundant $ do
    otherSrc <- tryIO NoPV $ Text.readFile $ moduleLocation otherModule
    otherProg <- parseSource (moduleLocation otherModule) otherSrc
    elaborate otherModule otherProg
