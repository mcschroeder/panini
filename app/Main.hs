{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Strict
import Panini.Elaborator
import Panini.REPL
import System.Console.Haskeline
import System.Directory
import System.Exit
import System.FilePath
import Prelude

main :: IO ()
main = do
  res <- runExceptT $ execStateT replMain initState
  case res of
    Left err -> do
      putStrLn $ "panic! at the repl: " ++ show err -- TODO: pretty?
      exitFailure
    Right _ -> return ()

replMain :: Elab ()
replMain = do
  configDir <- liftIO $ getXdgDirectory XdgConfig "panini"
  liftIO $ createDirectoryIfMissing True configDir
  let historyFile = configDir </> "repl_history"
  runInputT (replSettings (Just historyFile)) repl
