-- TODO: module documentation
module Panini.Solver.Z3 (smtValid) where

import Data.Char (isSpace)
import Control.Monad.IO.Class
import Data.List (dropWhileEnd)
import Data.Text qualified as Text
import Panini.Logger
import Panini.Monad
import Panini.Solver.SMTLIB
import Prelude
import System.Exit
import System.Process

-------------------------------------------------------------------------------

smtValid :: SMTLIB a => [a] -> Pan Bool
smtValid cs = do
  logMessage Trace "Z3" "Encode SMT-LIB query"
  let foralls = map (Text.unpack . toSMTLIB) cs
  let declares = []
  let asserts = map (\f -> "(assert " ++ f ++ ")") foralls
  let query = unlines $ declares ++ asserts ++ ["(check-sat)"]
  logData Trace query

  logMessage Debug "Z3" "Check satisfiability"
  (code, output, _) <- liftIO $ readProcessWithExitCode "z3" ["-smt2", "-in"] query
  logData Trace output

  case code of
    ExitSuccess -> case dropWhileEnd isSpace output of
      "sat"   -> return True
      "unsat" -> return False
      x       -> error x
    ExitFailure _ -> error output
