-- TODO: module documentation
module Panini.Solver.Z3 (smtValid) where

import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Data.Text qualified as Text
import Panini.Solver.SMTLIB
import Prelude
import System.Exit
import System.Process

-- TODO: encode string ops more safely/consistently

smtValid :: SMTLib2 a => [a] -> IO Bool
smtValid cs = do
  let foralls = map (Text.unpack . printSMTLib2) cs
  let declares = 
        [ "(define-fun length ((s String)) Int (str.len s))"
        , "(define-fun substring ((s String) (i Int) (n Int)) String (str.substr s i n))"
        -- "(declare-fun length ((String)) Int)"
        -- , "(declare-fun substring ((String) (Int)) String)"
        ]
  let asserts = map (\f -> "(assert " ++ f ++ ")") foralls
  let query = unlines $ declares ++ asserts ++ ["(check-sat)"]
  putStrLn query
  (code, output, _) <- readProcessWithExitCode "z3" ["-smt2", "-in"] query
  case code of
    ExitSuccess -> case dropWhileEnd isSpace output of
      "sat" -> return True
      "unsat" -> return False
      x -> error x    
    ExitFailure _ -> error output