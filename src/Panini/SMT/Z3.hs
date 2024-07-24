-- TODO: module documentation
module Panini.SMT.Z3 
  ( smtInit
  , smtCheck
  , smtCheckReversed
  , Result(..)
  , isSat
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
import Panini.Provenance
import Panini.SMT.SMTLIB
import Panini.SMT.ReversableCon
import Prelude
import System.Exit
import System.Process

-- TODO: add provenance to solver errors

-------------------------------------------------------------------------------

smtInit :: Pan ()
smtInit = do
  r <- liftIO $ try @IOException $ readProcessWithExitCode "z3" ["-version"] ""
  let solverError e = SolverError $ "Unable to initialize Z3:\n" <> Text.pack e
  case r of
    Left err -> throwError $ solverError (show err) NoPV
    Right (code, output, _) -> case code of
      ExitFailure _ -> throwError $ solverError output NoPV
      ExitSuccess -> do
        logEvent $ SMTSolverInitialized $ dropWhileEnd isSpace output
        timeout <- gets smtTimeout
        logMessage $ "Solver timeout:" <+> pretty timeout <+> "seconds"

-------------------------------------------------------------------------------

data Result = Sat | Unsat | Unknown String

isSat :: Result -> Bool
isSat Sat = True
isSat _   = False

smtCheck :: SMTLIB a => [a] -> Pan Result
smtCheck cs = do
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
      "sat"     -> return Sat
      "unsat"   -> return Unsat
      "unknown" -> return (Unknown "")
      "timeout" -> return (Unknown "timeout")
      x         -> throwError $ SolverError (Text.pack x) NoPV
    ExitFailure _ -> throwError $ SolverError (Text.pack output) NoPV


-- | Check the satisfiability of a list of reversed constraints
-- | I.e. the implications are filpped from p => q to (p and not q) and the result needs to be unsat.
smtCheckReversed :: (ReversableCon a) => [a] -> Pan Result
smtCheckReversed cs = do
  logMessage "Reverse encoding SMT-LIB query"
  let (rc, constDeclares) = reverseConToResult cs
  let foralls = map (Text.unpack . toSMTLIB) rc
  --dataSorts <- Map.keys <$> gets dataTypeEnvironemnt
  --let sorts = map (\f -> "(declare-sort " ++ Text.unpack ( getTextFromName f) ++ ")") dataSorts
  --let declareFuns =  ["(declare-fun len (list) Int)"] -- TODO: Fix declare-fun
  let consts = map (\(x, b) -> "(declare-const " ++ (Text.unpack . toSMTLIB) x ++ " " ++ (Text.unpack . toSMTLIB) b ++ ")") constDeclares
  let asserts = map (\f -> "(assert " ++ f ++ ")") foralls
  let query = unlines $  consts ++  asserts ++ ["(check-sat)"]
  result <- executeSMTQuery query
  case result of 
    -- Filp the result. If the reversed constraints are sat, the original constraints are unsat. (p and not q) sat means p => q unsat
    Sat -> return Unsat 
    Unsat -> return Sat
    Unknown u -> return (Unknown u)


-- | executes a query and returns the result
-- | The Query is expected to be in SMT-LIB format
-- | Helper to allow calling in multiple settings
executeSMTQuery :: String -> Pan Result
executeSMTQuery query = do
  logData query

  timeout <- gets smtTimeout
  let args = ["-T:" ++ show timeout]
  logMessage $ "Check satisfiability" <+> pretty args    
  (code, output, _) <- liftIO $ readProcessWithExitCode "z3" (["-smt2", "-in"] ++ args) query
  logData output

  case code of
    ExitSuccess -> case dropWhileEnd isSpace output of
      "sat"     -> return Sat
      "unsat"   -> return Unsat
      "unknown" -> return (Unknown "")
      "timeout" -> return (Unknown "timeout")
      x         -> throwError $ SolverError (Text.pack x) NoPV
    ExitFailure _ -> throwError $ SolverError (Text.pack output) NoPV