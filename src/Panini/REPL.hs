module Panini.REPL
  ( repl,
    replSettings,
  )
where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Strict
import Data.Char (isSpace, toLower)
import Data.List (isPrefixOf)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Panini.Elaborator
import Panini.Error
import Panini.Parser
import Panini.Printer
import Panini.Syntax
import Panini.TypeChecker
import Prelude
import System.Console.ANSI
import System.Console.Haskeline
import System.IO

-------------------------------------------------------------------------------

-- | Panini REPL.
repl :: InputT Panini ()
repl = do
  let prompt = "Panini> "
  let byeMsg = "byeee 👋"
  x <- getInputLine prompt
  case x of
    Nothing -> outputStrLn byeMsg
    Just input -> case parseCmd input of
      Left err -> outputStrLn err >> repl
      Right cmd -> case cmd of
        Quit        -> outputStrLn byeMsg
        Format s    -> formatInput s      >> repl
        TypeSynth s -> synthesizeType s   >> repl
        Load fs     -> loadFiles fs       >> repl
        Eval expr   -> evaluateInput expr >> repl

formatInput :: String -> InputT Panini ()
formatInput = mapM_ output . parseInput @Expr

synthesizeType :: String -> InputT Panini ()
synthesizeType input = do
  g <- lift $ gets pan_types
  case synth g =<< parseInput input of
    Left err -> output err
    Right (vc, t) -> do
      output vc
      output t

evaluateInput :: String -> InputT Panini ()
evaluateInput input = do
  res <- lift $ tryError $ elabDecl =<< lift (except $ parseInput input)  
  case res of
    Left err -> output err
    Right () -> return ()

loadFiles :: [FilePath] -> InputT Panini ()
loadFiles fs = forM_ fs $ \f -> do
  src <- liftIO $ Text.readFile f
  case parseProg f src of
    Left err1 -> output err1
    Right prog -> do
      res <- lift $ tryError $ elabProg prog
      case res of
        Left err2 -> output err2
        Right () -> return ()

-------------------------------------------------------------------------------

data Command
  = Quit
  | Format String
  | TypeSynth String
  | Eval String
  | Load [String]
  deriving stock (Show, Read)

parseCmd :: String -> Either String Command
parseCmd (':' : input) = case break isSpace input of
  (map toLower -> cmd, dropWhile isSpace -> args)
    | cmd `isPrefixOf` "quit" -> Right Quit
    | cmd `isPrefixOf` "format" -> Right (Format args)
    | cmd `isPrefixOf` "type" -> Right (TypeSynth args)
    | cmd `isPrefixOf` "load" -> Right $ Load (words args)
    | otherwise -> Left ("unknown command :" ++ cmd)
parseCmd input = Right (Eval input)

-------------------------------------------------------------------------------

-- | Panini REPL settings with working autocomplete.
replSettings :: Maybe FilePath -> Settings Panini
replSettings histFile =
  Settings
    { complete = autocomplete,
      historyFile = histFile,
      autoAddHistory = True
    }

autocomplete :: CompletionFunc Panini
autocomplete = completeWord' Nothing isSpace $ \str -> do
  if null str
    then return []
    else return $ map simpleCompletion $ filter (str `isPrefixOf`) cmds
  where
    cmds = [":quit", ":format", ":type"]

-------------------------------------------------------------------------------

class Inputable a where
  parseInput :: String -> Either Error a

instance Inputable Expr where
  parseInput = parseExpr "<repl>" . Text.pack

instance Inputable Decl where
  parseInput = parseDecl "<repl>" . Text.pack

-------------------------------------------------------------------------------

class Outputable a where
  renderOutput :: PrintOptions -> a -> Text

instance Outputable Expr where
  renderOutput = printExpr

instance Outputable Con where
  renderOutput = printCon

instance Outputable Type where
  renderOutput = printType

instance Outputable Error where
  renderOutput opts = printError opts "<repl>"

output :: Outputable a => a -> InputT Panini ()
output x = do
  ansiColors <- liftIO $ hSupportsANSIColor stdout
  fixedWidth <- liftIO $ fmap snd <$> getTerminalSize
  let opts = PrintOptions {unicodeSymbols = True, ansiColors, fixedWidth}
  outputStrLn $ Text.unpack $ renderOutput opts x
