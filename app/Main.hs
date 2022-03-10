{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.IO.Class
import Control.Monad.Trans.State.Strict
import Panini.REPL
import Prelude
import System.Console.Haskeline
import System.Directory
import System.FilePath

main :: IO ()
main = do
  _ <- execStateT paniniMain paniniInitState
  return ()

paniniMain :: Panini ()
paniniMain = do
  configDir <- liftIO $ getXdgDirectory XdgConfig "panini"
  liftIO $ createDirectoryIfMissing True configDir
  let historyFile = configDir </> "repl_history"
  runInputT (replSettings (Just historyFile)) repl
