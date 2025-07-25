-- TODO: module documentation
module Panini.SMT.Z3 
  ( smtInit
  , smtCheck
  , Result(..)
  , isSat
  ) where

import Control.Exception
import Control.Monad.IO.Class
import Control.Monad.Trans.State.Strict
import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Data.Text qualified as Text
import Panini.Monad
import Panini.Pretty
import Panini.SMT.Error
import Panini.SMT.SMTLIB
import Prelude
import System.Exit
import System.Process

-- TODO: add provenance to solver errors

-------------------------------------------------------------------------------

smtInit :: Pan Error ()
smtInit = do
  r <- liftIO $ try @IOException $ readProcessWithExitCode "z3" ["-version"] ""
  case r of
    Left err -> throwError $ InitError Nothing (show err)
    Right (code, output, _) -> case code of
      ExitFailure c -> throwError $ InitError (Just c) output
      ExitSuccess -> info $ pretty $ dropWhileEnd isSpace output

-------------------------------------------------------------------------------

data Result = Sat | Unsat | Unknown String

isSat :: Result -> Bool
isSat Sat = True
isSat _   = False

smtCheck :: SMTLIB a => [a] -> Pan Error Result
smtCheck cs = do
  let foralls = map (Text.unpack . toSMTLIB) cs
  let declares = []
  let asserts = map (\f -> "(assert " ++ f ++ ")") foralls
  let query = unlines $ declares ++ asserts ++ ["(check-sat)"]
  timeout <- gets smtTimeout
  let args = ["-T:" ++ show timeout]
  
  info @Doc "Query SMT solver"
  trace $ pretty query
  (code, output, _) <- liftIO $ readProcessWithExitCode "z3" (["-smt2", "-in"] ++ args) query
  info @Doc "Received SMT solver response"
  trace $ pretty output

  case code of
    ExitSuccess -> case dropWhileEnd isSpace output of
      "sat"     -> return Sat
      "unsat"   -> return Unsat
      "unknown" -> return (Unknown "")
      "timeout" -> return (Unknown "timeout")
      x         -> throwError $ QueryError args query Nothing x
    ExitFailure c -> throwError $ QueryError args query (Just c) output
