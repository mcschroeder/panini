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
import Panini.SMT.SMTLIB
import Prelude
import System.Exit
import System.Process
import Panini.Environment (SmtError(..))
import Panini.Pretty

-- TODO: add provenance to solver errors

-------------------------------------------------------------------------------

smtInit :: Pan SmtError ()
smtInit = do
  r <- liftIO $ try @IOException $ readProcessWithExitCode "z3" ["-version"] ""
  case r of
    Left err -> throwError $ SmtError (show err)
    Right (code, output, _) -> case code of
      ExitFailure _ -> throwError $ SmtError output
      ExitSuccess -> do
        let version = dropWhileEnd isSpace output
        info $ pretty version

-------------------------------------------------------------------------------

data Result = Sat | Unsat | Unknown String

isSat :: Result -> Bool
isSat Sat = True
isSat _   = False

smtCheck :: SMTLIB a => [a] -> Pan SmtError Result
smtCheck cs = do
  let foralls = map (Text.unpack . toSMTLIB) cs
  let declares = []
  let asserts = map (\f -> "(assert " ++ f ++ ")") foralls
  let query = unlines $ declares ++ asserts ++ ["(check-sat)"]
  timeout <- gets smtTimeout
  let args = ["-T:" ++ show timeout]
  
  trace $ pretty query
  (code, output, _) <- liftIO $ readProcessWithExitCode "z3" (["-smt2", "-in"] ++ args) query
  trace $ pretty output

  case code of
    ExitSuccess -> case dropWhileEnd isSpace output of
      "sat"     -> return Sat
      "unsat"   -> return Unsat
      "unknown" -> return (Unknown "")
      "timeout" -> return (Unknown "timeout")
      x         -> throwError $ SmtError x
    ExitFailure _ -> throwError $ SmtError output
