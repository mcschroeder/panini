-- TODO: module documentation
module Panini.Solver.Z3 (smtValid) where

import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Data.Text qualified as Text
import Panini.Logger
import Panini.Solver.SMTLIB
import Prelude
import System.Exit
import System.Process
import Control.Monad.IO.Class

smtValid :: (MonadIO m, HasLogger m) => SMTLIB a => [a] -> m Bool
smtValid cs = do
  query <- logTimeM Trace "Z3" "Encode SMTLIB query" $ do
    let foralls = map (Text.unpack . toSMTLIB) cs
    let declares = []
    let asserts = map (\f -> "(assert " ++ f ++ ")") foralls
    return $ unlines $ declares ++ asserts ++ ["(check-sat)"]
  logData Trace query
  (code, output, _) <- logTimeM Info "Z3" "Solve query with Z3" $ 
    liftIO $ readProcessWithExitCode "z3" ["-smt2", "-in"] query
  case code of
    ExitSuccess -> case dropWhileEnd isSpace output of
      "sat" -> return True
      "unsat" -> return False
      x -> error x    
    ExitFailure _ -> error output