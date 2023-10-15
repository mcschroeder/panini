-- TODO: module documentation
module Panini.SMT.Z3 
  ( smtInit
  , smtValid
  ) where

import Control.Exception
import Control.Monad.IO.Class
import Control.Monad.Trans.State.Strict
import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Data.Text qualified as Text
import Panini.Error
import Panini.Events
import Panini.Monad
import Panini.Pretty
import Panini.SMT.SMTLIB
import Prelude
import System.Exit
import System.Process

-------------------------------------------------------------------------------

smtInit :: Pan ()
smtInit = do
  r <- liftIO $ try @IOException $ readProcessWithExitCode "z3" ["-version"] ""
  let solverError e = SolverError $ "Unable to initialize Z3:\n" <> Text.pack e
  case r of
    Left err -> throwError $ solverError $ show err
    Right (code, output, _) -> case code of
      ExitFailure _ -> throwError $ solverError output
      ExitSuccess -> do
        logEvent $ SMTSolverInitialized $ dropWhileEnd isSpace output
        timeout <- gets smtTimeout
        logMessage $ "Solver timeout:" <+> pretty timeout <+> "seconds"

smtValid :: SMTLIB a => [a] -> Pan Bool
smtValid cs = do
  logMessage "Encode SMT-LIB query"
  let foralls = map (Text.unpack . toSMTLIB) cs
  let declares = []
  let asserts = map (\f -> "(assert " ++ f ++ ")") foralls
  let query = unlines $ declares ++ asserts ++ ["(check-sat)"]
  logData query

  timeout <- gets smtTimeout
  let args = ["-T:" ++ show timeout]
  logMessage $ "Check satisfiability" <+> pretty args    
  (code, output, _) <- liftIO $ readProcessWithExitCode "z3" (["-smt2", "-in"] ++ args) query
  logData output

  case code of
    ExitSuccess -> case dropWhileEnd isSpace output of
      "sat"   -> return True
      "unsat" -> return False
      x       -> throwError $ SolverError $ Text.pack x
    ExitFailure _ -> throwError $ SolverError $ Text.pack output

