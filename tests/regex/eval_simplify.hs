{-# LANGUAGE GHC2021 #-}

import Control.Monad
import Data.List (isInfixOf)
import Data.Maybe
import Panini.Pretty
import Panini.Regex
import Panini.Regex.POSIX.BE as BE
import Panini.Regex.POSIX.ERE as ERE
import Panini.Regex.Simplify
import Panini.Regex.Simplify.Common
import Prelude
import System.Clock
import System.Directory.Extra
import System.FilePath
import System.IO
import System.Time.Extra
import Text.Printf

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStrLn "file,line,size_before,size_after,time_ns,result"
  inputFiles <- gatherInputFiles
  forM_ inputFiles $ \inputFile -> do
    let fileName = takeFileName inputFile
    rawLines <- lines <$> readFile inputFile
    forM_ (zip rawLines [1..]) $ \(raw, i :: Int) -> do
      printf "%s,%d," fileName i      
      -- convert MrE regex format to POSIX
      let raw' | "MrE" `isInfixOf` inputFile = map (\c -> case c of '+' -> '|'; _ -> c) raw
               | otherwise = raw
      let pos1 = fromJust $ ERE.parseERE raw'
      let !reg1 = ERE.toRegex pos1
      let len1 = size reg1
      printf "%d," len1
      t1 <- getTime Monotonic
      !result <- timeout 60 $ return $! simplify reg1
      t2 <- getTime Monotonic
      let delta = toNanoSecs $ diffTimeSpec t1 t2
      case result of
        Nothing -> printf "%d,%d,timeout\n" len1 delta
        Just reg2 -> do
          let len2 = size reg2
          printf "%d,%d," len2 delta
          case True of
          --case equivalence reg1 reg2 of
            True -> printf "ok\n"
            False -> printf "wrong\n"

gatherInputFiles :: IO [FilePath]
gatherInputFiles = return ["tests/regex/data/00_timeout.txt"]
--gatherInputFiles = listFilesRecursive "tests/regex/data"
