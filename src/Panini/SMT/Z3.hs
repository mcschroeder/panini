-- TODO: module documentation
module Panini.SMT.Z3 
  ( smtInit
  , smtValid
  ) where

import Control.Exception
import Control.Monad.IO.Class
import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Data.Text qualified as Text
import Panini.Error
import Panini.Events
import Panini.Monad
import Panini.SMT.SMTLIB
import Prelude
import System.Exit
import System.Process

-- TODO: make solver timeout adjustable

-------------------------------------------------------------------------------

smtInit :: Pan ()
smtInit = do
  r <- liftIO $ try @IOException $ readProcessWithExitCode "z3" ["-version"] ""
  let solverError e = SolverError $ "Unable to initialize Z3:\n" <> Text.pack e
  case r of
    Left err -> throwError $ solverError $ show err
    Right (code, output, _) -> case code of
      ExitFailure _ -> throwError $ solverError output
      ExitSuccess -> 
        logEvent $ SMTSolverInitialized $ dropWhileEnd isSpace output

smtValid :: SMTLIB a => [a] -> Pan Bool
smtValid cs = do
  logMessage "Encode SMT-LIB query"
  let foralls = map (Text.unpack . toSMTLIB) cs
  let declares = []
  let asserts = map (\f -> "(assert " ++ f ++ ")") foralls
  let query = unlines $ declares ++ asserts ++ ["(check-sat)"]
  logData query

  logMessage "Check satisfiability"
  (code, output, _) <- liftIO $ readProcessWithExitCode "z3" ["-smt2", "-in", "-T:5"] query
  logData output

  case code of
    ExitSuccess -> case dropWhileEnd isSpace output of
      "sat"   -> return True
      "unsat" -> return False
      x       -> throwError $ SolverError $ Text.pack x
    ExitFailure _ -> throwError $ SolverError $ Text.pack output

