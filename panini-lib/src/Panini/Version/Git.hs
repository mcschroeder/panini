{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module Panini.Version.Git where

import Control.Exception (try, SomeException)
import Control.Monad.Trans.Maybe
import Data.Maybe
import Language.Haskell.TH
import Prelude
import System.Environment
import System.Exit
import System.Process

gitString :: Q Exp
gitString = runIO $ do
  wd <- fromMaybe "." <$> lookupEnv "GIT_ROOT"
  getGitInfo wd >>= \case
    Just gi 
      | dirty gi  -> litE $ stringL $ describe gi ++ "-dirty"
      | otherwise -> litE $ stringL $ describe gi
    Nothing       -> litE $ stringL "out-of-tree build"

data GitInfo = GitInfo { describe :: String, dirty :: Bool }

getGitInfo :: FilePath -> IO (Maybe GitInfo)
getGitInfo cwd = runMaybeT $ do
  describe    <- MaybeT $ runGit cwd ["describe", "--always"]
  dirtyString <- MaybeT $ runGit cwd ["status", "--porcelain"]
  let dirty    = not $ null (dirtyString :: String)
  return       $ GitInfo { describe, dirty }

runGit :: FilePath -> [String] -> IO (Maybe String)
runGit cwd args = do
  let cp = (proc "git" args) { cwd = Just cwd }
  result <- try (readCreateProcessWithExitCode cp "")
  case result of    
    Left (_ :: SomeException)   -> return Nothing
    Right (ExitSuccess, out, _) -> return $ Just $ takeWhile (/= '\n') out
    _                           -> return Nothing
