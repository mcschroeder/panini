-- TODO: module documentation
module Panini.SMT.Z3 (smtValid) where

import Data.Char (isSpace)
import Control.Monad.IO.Class
import Data.List (dropWhileEnd)
import Data.Text qualified as Text
import Panini.Error
import Panini.Logger
import Panini.Monad
import Panini.SMT.SMTLIB
import Prelude
import System.Exit
import System.Process

-------------------------------------------------------------------------------

smtValid :: SMTLIB a => [a] -> Pan Bool
smtValid cs = do
  logMessage "Z3" "Encode SMT-LIB query"
  let foralls = map (Text.unpack . toSMTLIB) cs
  let declares = []
  let asserts = map (\f -> "(assert " ++ f ++ ")") foralls
  let query = unlines $ declares ++ asserts ++ ["(check-sat)"]
  logData "SMT-LIB Query" query

  logMessage "Z3" "Check satisfiability"
  (code, output, _) <- liftIO $ readProcessWithExitCode "z3" ["-smt2", "-in"] query
  logData "Z3 Output" output

  case code of
    ExitSuccess -> case dropWhileEnd isSpace output of
      "sat"   -> return True
      "unsat" -> return False
      x       -> throwError $ SolverError $ Text.pack x
    ExitFailure _ -> throwError $ SolverError $ Text.pack output
