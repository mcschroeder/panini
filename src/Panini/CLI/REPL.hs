module Panini.CLI.REPL
  ( repl,
    replSettings,
  )
where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import Data.Bifunctor
import Data.Char (isSpace, toLower)
import Data.Function
import Data.List (isPrefixOf, groupBy, sortOn, inits)
import Data.Map qualified as Map
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Panini.Elab
import Panini.Environment
import Panini.Monad
import Panini.Parser
import Panini.Pretty.Printer
import Panini.Provenance
import Prelude
import System.Console.ANSI
import System.Console.Haskeline
import System.IO

-------------------------------------------------------------------------------

-- | Panini REPL.
repl :: InputT Pan ()
repl = loop
  where
    prompt = "Panini> "
    byeMsg = "byeee 👋"
    multiMsg    = "╭── Entering multi-line mode. Press ⌃D to finish."
    multiPrompt = "│ "
    multiMsgEnd = "╰──"
    -- TODO: ASCII mode for multi-line chrome
    -- TODO: colorize multi-line chrome?

    loop = do
      minput <- fmap (dropWhile isSpace) <$> getInputLine prompt
      handleInput minput

    loopMultiline xs = do
      minput <- getInputLine multiPrompt
      case minput of
        Just x  -> loopMultiline (x:xs)
        Nothing -> do
          outputStrLn multiMsgEnd
          handleInput $ Just $ unlines $ reverse xs

    handleInput = \case
      Nothing -> outputStrLn byeMsg
      Just "" -> loop
      Just (':':(splitCmd -> (cmd,args)))
        | cmd `isPrefixOf` "quit"  -> handleInput Nothing
        | cmd `isPrefixOf` "paste" -> outputStrLn multiMsg >> loopMultiline []
        | Just f <- lookup cmd commandPrefixes -> f args >> loop
        | otherwise -> outputStrLn ("unknown command :" ++ cmd) >> loop
      Just input -> evaluateInput input >> loop

    splitCmd = bimap (map toLower) (dropWhile isSpace) . break isSpace
    
    commandPrefixes = map head $ groupBy ((==) `on` fst) $ sortOn fst $ concat
                    $ map (uncurry zip . bimap (tail . inits) repeat) commands
    
-------------------------------------------------------------------------------

commands :: [(String, String -> InputT Pan ())]
commands = 
  [ ("help", const help)
  , ("load", loadFiles . words)
  , ("show", const showEnv)
  ]

help :: InputT Pan ()
help = outputStrLn "\
  \Commands available in the REPL:\n\
  \\n\
  \  <statement>          Evaluate the given statement(s)\n\
  \  :help                Display this list of commands\n\
  \  :load <file> ...     Load file(s) into the environment\n\
  \  :paste               Enter multi-line mode\n\
  \  :quit                Exit the REPL\n\
  \  :show                Show all bindings in the environment\n\
  \"

loadFiles :: [String] -> InputT Pan ()
loadFiles = mapM_ loadFile

-- TODO: add source lines to error PV
loadFile :: FilePath -> InputT Pan ()
loadFile f = lift $ continueOnError $ do
  src <- tryIO NoPV $ Text.readFile f
  prog <- parseSource f src
  elaborateProgram f prog
  -- TODO: output summary like "Ok, 23 modules loaded."

-- TODO
showEnv :: InputT Pan ()
showEnv = do
  env <- lift get
  forM_ (Map.toAscList env.environment) $ \case
    (_,Assumed{_name,_givenType}) -> outputStrLn $ showPretty $ 
      pretty _name <+> symColon <+> pretty _givenType    
    (_,Verified{_name,_solvedType}) -> outputStrLn $ showPretty $ 
      pretty _name <+> symColon <+> pretty _solvedType

-- TODO: add source lines to error PV
evaluateInput :: String -> InputT Pan ()
evaluateInput input = lift $ continueOnError $ do
  let src = Text.pack input
  prog <- parseSource "<repl>" src
  elaborateProgram "<repl>" prog

-------------------------------------------------------------------------------

-- | Panini REPL settings with working autocomplete.
replSettings :: Maybe FilePath -> Settings Pan
replSettings histFile =
  Settings
    { complete = autocomplete,
      historyFile = histFile,
      autoAddHistory = True
    }

-- TODO: improve this hacky implementation
autocomplete :: CompletionFunc Pan
autocomplete = fallbackCompletion completeCommands completeFiles
  where
    completeCommands = completeWord' Nothing isSpace $ \str -> do
      if null str
        then return []
        else return $ map simpleCompletion $ filter (str `isPrefixOf`) cmds

    cmds = [":quit", ":paste"] ++ map ((':':) . fst) commands

    completeFiles (r,s) = case splitCmd (reverse r) of
      (":load", _) -> completeFilename (r,s)
      (":grammar", _) -> completeFilename (r,s)
      _ -> pure (r,[])
    
    splitCmd = bimap (map toLower) (dropWhile isSpace) . break isSpace

-------------------------------------------------------------------------------

addSourceLinesREPL :: String -> PV -> IO PV
addSourceLinesREPL input (FromSource loc Nothing) | loc.file == "<repl>" = do
  let src = head $ lines $ extractLines loc.begin loc.end input
  pure $ FromSource loc (Just $ Text.pack src)
addSourceLinesREPL input (Derived pv x) = do
  pv' <- addSourceLinesREPL input pv
  return $ Derived pv' x
addSourceLinesREPL _ pv = addSourceLines pv

extractLines :: (Int,Int) -> (Int,Int) -> String -> String
extractLines (l1,_) (l2,_) = 
  unlines . take (l2 + 1 - l1) . drop (l1 - 1) . lines

outputPretty :: Pretty a => a -> InputT Pan ()
outputPretty x = do
  ansiColors <- liftIO $ hSupportsANSIColor stdout
  let styling = if ansiColors then Just defaultStyling else Nothing
  fixedWidth <- liftIO $ fmap snd <$> getTerminalSize  
  let opts = RenderOptions {unicode = True, styling, fixedWidth}
  outputStrLn $ Text.unpack $ renderDoc opts $ pretty x
