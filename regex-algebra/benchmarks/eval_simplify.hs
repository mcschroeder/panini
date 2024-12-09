{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE ViewPatterns #-}

import Control.Monad
import Control.Monad.Combinators.NonEmpty qualified as NE
import Data.Foldable
import Data.IORef
import Data.List (isInfixOf)
import Data.List.NonEmpty qualified as NE
import Data.Maybe
import Data.Vector qualified as V
import Data.Vector.Mutable qualified as VM
import Data.Vector.Unboxed.Mutable qualified as VUM
import Data.Void
import Prelude hiding (minimum, maximum)
import Regex
import Regex.POSIX.BE as BE
import Regex.POSIX.ERE as ERE
import Regex.Simplify
import Regex.Simplify.Common
import Statistics.Quantile
import Statistics.Sample
import System.Clock
import System.Directory.Extra
import System.Environment
import System.FilePath
import System.IO
import System.Time.Extra
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L
import Text.Printf

defaultTimeout :: Seconds
defaultTimeout = 30

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering

  args <- getArgs
  let args' = filter (/= "--verify") args
  let verify = length args /= length args'
  when verify $ putStrLn "[verification enabled]"

  inputFiles <- gatherInputFiles args'

  putStrLn ""
  putStrLn "                                         time (ms)             output size"
  putStrLn "                               ┌───────────────────────────┬──────────────────┐"
  putStrLn "                        ✓    - │  median     mean       sd │ ×minimum  ×input │"
  putStrLn "───────────────────────────────┼───────────────────────────┼──────────────────┤"

  numOkTotalRef <- newIORef (0 :: Int)
  numTimeoutsTotalRef <- newIORef (0 :: Int)
  timesTotalRef <- newIORef []
  sizeExcessTotalRef <- newIORef []
  sizeShrinkageTotalRef <- newIORef []
  timesOkTotalRef <- newIORef []
  sizeExcessOkTotalRef <- newIORef []
  sizeShrinkageOkTotalRef <- newIORef []

  forM_ inputFiles $ \inputFile -> do
    let fileName = takeFileName inputFile
    rawLines <- lines <$> readFile inputFile
    let n = length rawLines

    numTimeoutsRef <- newIORef (0 :: Int)
    timesRef <- newIORef []
    sizeExcessRef <- newIORef []
    sizeShrinkageRef <- newIORef []

    printf "%s" fileName

    forM_ (zip rawLines [1..]) $ \(rawLine, i :: Int) -> do
      let regexFormat = guessFormat inputFile
      let !regBefore = parseRegex regexFormat rawLine
      let !sizeBefore = size regBefore      
      t1 <- getTime Monotonic
      !result <- timeout defaultTimeout $ return $! simplify regBefore
      t2 <- getTime Monotonic
      let testTimeMillis = (toNanoSecs $ diffTimeSpec t1 t2) ./. 1000000
      let !sizeAfter | Just regAfter <- result = size regAfter
                     | otherwise = sizeBefore
      let sizeMinimal = min sizeAfter <$> guessMinimalSize inputFile
      let sizeExcess = maybe 0 (sizeAfter ./.) sizeMinimal
      let sizeShrinkage = sizeAfter ./. sizeBefore

      modifyIORef' timesRef (testTimeMillis:)
      modifyIORef' sizeExcessRef (sizeExcess:)
      modifyIORef' sizeShrinkageRef (sizeShrinkage:)

      modifyIORef' timesTotalRef (testTimeMillis:)
      modifyIORef' sizeExcessTotalRef (sizeExcess:)
      modifyIORef' sizeShrinkageTotalRef (sizeShrinkage:)      

      case result of
        Just regAfter | verify, not $ equivalence regBefore regAfter -> 
          error $ printf "unsound simplification for %s:%d" inputFile i

        Just regAfter -> do
          modifyIORef' numOkTotalRef (+1)
          modifyIORef' timesOkTotalRef (testTimeMillis:)
          modifyIORef' sizeExcessOkTotalRef (sizeExcess:)
          modifyIORef' sizeShrinkageOkTotalRef (sizeShrinkage:)

        Nothing -> do
          let loc :: String = printf "%s:%d" fileName i
          printf "\r%-30s ┆       -        -        - ┆        -       - ┆\n" loc
          modifyIORef' numTimeoutsRef (+1)
          modifyIORef' numTimeoutsTotalRef (+1)

      times <- V.fromList <$> readIORef timesRef
      sizeExcess <- V.fromList <$> readIORef sizeExcessRef
      sizeShrinkage <- V.fromList <$> readIORef sizeShrinkageRef

      let minTime = V.minimum times
      let (meanTime, varTime) = meanVarianceUnb times
      let sdTime = sqrt varTime
      let medianTime = median s times
      let maxTime = V.maximum times
      
      numTimeouts <- readIORef numTimeoutsRef
      let numOk = i - numTimeouts
      let geomeanExcess = geometricMean sizeExcess
      let geomeanShrinkage = geometricMean sizeShrinkage 

      printf "\r%-20s %4d %4d │ %7.2f  %7.2f  %7.2f │ %8.4f  %6.4f │" 
        fileName numOk numTimeouts 
        medianTime meanTime sdTime 
        geomeanExcess geomeanShrinkage
    
    printf "\n"
  
  putStrLn "───────────────────────────────┼───────────────────────────┼──────────────────┤"

  times <- V.fromList <$> readIORef timesTotalRef
  sizeExcess <- V.fromList <$> readIORef sizeExcessTotalRef
  sizeShrinkage <- V.fromList <$> readIORef sizeShrinkageTotalRef

  let minTime = V.minimum times
  let (meanTime, varTime) = meanVarianceUnb times
  let sdTime = sqrt varTime
  let medianTime = median s times
  let maxTime = V.maximum times
      
  numTimeouts <- readIORef numTimeoutsTotalRef
  numOk <- readIORef numOkTotalRef
  let geomeanExcess = geometricMean sizeExcess
  let geomeanShrinkage = geometricMean sizeShrinkage 

  printf "%-19s %5d %4d │ %7.2f  %7.2f  %7.2f │ %8.4f  %6.4f │\n"
    "total" numOk numTimeouts 
    medianTime meanTime sdTime
    geomeanExcess geomeanShrinkage

  when (numTimeouts > 0) $ do
    timesOk <- V.fromList <$> readIORef timesOkTotalRef
    sizeExcessOk <- V.fromList <$> readIORef sizeExcessOkTotalRef
    sizeShrinkageOk <- V.fromList <$> readIORef sizeShrinkageOkTotalRef

    let minTimeOk = V.minimum timesOk
    let (meanTimeOk, varTimeOk) = meanVarianceUnb timesOk
    let sdTimeOk = sqrt varTimeOk
    let medianTimeOk = median s timesOk
    let maxTimeOk = V.maximum timesOk      
    let geomeanExcessOk = geometricMean sizeExcessOk
    let geomeanShrinkageOk = geometricMean sizeShrinkageOk

    printf "%-19s %5d %4d │ %7.2f  %7.2f  %7.2f │ %8.4f  %6.4f │\n"
      "total w/o timeouts" numOk (0 :: Int)
      medianTimeOk meanTimeOk sdTimeOk
      geomeanExcessOk geomeanShrinkageOk


(./.) :: Integral a => a -> a -> Double
a ./. b = fromIntegral a / fromIntegral b

data TestStatus = Ok | Wrong | Timeout

guessMinimalSize :: FilePath -> Maybe Int
guessMinimalSize inputFile
  | "expansions" `isInfixOf`inputFile
  , ('s': (reads @Int -> [(s,_)])) <- takeFileName inputFile = Just s
guessMinimalSize _ = Nothing

data RegexFormat = MrE | POSIX

guessFormat :: FilePath -> RegexFormat
guessFormat inputFile 
  | "MrE" `isInfixOf` inputFile = MrE
  | otherwise = POSIX

parseRegex :: RegexFormat -> String -> Regex
parseRegex MrE   = ERE.toRegex . fromJust . parseMrE
parseRegex POSIX = ERE.toRegex . fromJust . ERE.parseERE

parseMrE :: String -> Maybe ERE
parseMrE = parseMaybe @Void mre
 where
  mre :: (MonadParsec e s m, Token s ~ Char) => m ERE
  mre = Alt <$> NE.sepBy1 con (char '+')
   where
    con = Con <$> NE.some exp
    exp = do
      e <- choice [chr, grp]
      ds <- many $ choice [ast, que]
      return $ foldl' Dup e ds
    chr = Chr <$> satisfy (`notElem` ("+()*?" :: [Char]))
    grp = Grp <$ char '(' <*> mre <* char ')'
    ast = Ast <$ char '*'
    que = Que <$ char '?'

gatherInputFiles :: [String] -> IO [FilePath]
gatherInputFiles [] = return []
gatherInputFiles (x:xs) = do
  isFile <- doesFileExist x
  inputs <- if isFile then return [x] else listFilesRecursive x
  (inputs ++) <$> gatherInputFiles xs
