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
import Data.Map (Map)
import Data.Map qualified as Map
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
repl :: InputT Elab ()
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
        Show        -> showState          >> repl

formatInput :: String -> InputT Elab ()
formatInput = mapM_ output . parseInput @Expr

synthesizeType :: String -> InputT Elab ()
synthesizeType input = do
  g <- lift $ gets pan_types
  case synth g =<< parseInput input of
    Left err -> output err
    Right (vc, t) -> do
      output vc
      output t

evaluateInput :: String -> InputT Elab ()
evaluateInput input = do
  res <- lift $ tryError $ elabDecl =<< lift (except $ parseInput input)  
  case res of
    Left err -> output err
    Right () -> return ()

loadFiles :: [FilePath] -> InputT Elab ()
loadFiles fs = forM_ fs $ \f -> do
  src <- liftIO $ Text.readFile f
  case parseProg f src of
    Left err1 -> output err1
    Right prog -> do
      res <- lift $ tryError $ elabProg prog
      case res of
        Left err2 -> output err2
        Right () -> return ()

showState :: InputT Elab ()
showState = do
  ElabState{pan_types, pan_terms, pan_vcs} <- lift get
  outputStrLn "\ntyping context"
  output pan_types
  outputStrLn "\nterm context"
  output pan_terms
  outputStrLn "\nverification conditions"
  output pan_vcs

-------------------------------------------------------------------------------

data Command
  = Quit
  | Format String
  | TypeSynth String
  | Eval String
  | Load [String]
  | Show
  deriving stock (Show, Read)

parseCmd :: String -> Either String Command
parseCmd (':' : input) = case break isSpace input of
  (map toLower -> cmd, dropWhile isSpace -> args)
    | cmd `isPrefixOf` "quit" -> Right Quit
    | cmd `isPrefixOf` "format" -> Right (Format args)
    | cmd `isPrefixOf` "type" -> Right (TypeSynth args)
    | cmd `isPrefixOf` "load" -> Right $ Load (words args)
    | cmd `isPrefixOf` "show" -> Right Show
    | otherwise -> Left ("unknown command :" ++ cmd)
parseCmd input = Right (Eval input)

-------------------------------------------------------------------------------

-- | Panini REPL settings with working autocomplete.
replSettings :: Maybe FilePath -> Settings Elab
replSettings histFile =
  Settings
    { complete = autocomplete,
      historyFile = histFile,
      autoAddHistory = True
    }

autocomplete :: CompletionFunc Elab
autocomplete = completeWord' Nothing isSpace $ \str -> do
  if null str
    then return []
    else return $ map simpleCompletion $ filter (str `isPrefixOf`) cmds
  where
    cmds = [":quit", ":format", ":type", ":load", ":show"]

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

instance Outputable (Map Name Type) where
  renderOutput opts = prettyPrint opts . pTypeCtx

instance Outputable (Map Name Expr) where
  renderOutput opts = prettyPrint opts . pTermCtx

instance Outputable (Map Name Con) where
  renderOutput opts = prettyPrint opts . pConCtx

output :: Outputable a => a -> InputT Elab ()
output x = do
  ansiColors <- liftIO $ hSupportsANSIColor stdout
  fixedWidth <- liftIO $ fmap snd <$> getTerminalSize
  let opts = PrintOptions {unicodeSymbols = True, ansiColors, fixedWidth}
  outputStrLn $ Text.unpack $ renderOutput opts x
