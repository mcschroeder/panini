module Panini.CLI.REPL
  ( replMain
  , repl
  , replSettings
  )
where

import Control.Monad.Extra hiding (loop)
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import Data.Bifunctor
import Data.Char (isSpace, toLower)
import Data.Function
import Data.List (isPrefixOf, groupBy, sortOn, inits)
import Data.List qualified as List
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Panini.CLI.Common
import Panini.CLI.Options
import Panini.Elab
import Panini.Environment
import Panini.Events
import Panini.Frontend.Python
import Panini.Monad
import Panini.Pretty
import Panini.Provenance
import Panini.SMT.Z3
import Panini.Version
import Prelude
import System.Console.Haskeline
import System.Directory
import System.Exit
import System.FilePath
import System.IO

-- TODO: add source lines to PV for <repl> module sources

-------------------------------------------------------------------------------

replMain :: PanOptions -> IO ()
replMain panOpts = do
  whenJust panOpts.outputFile $ \_ ->
      putStrLn $ "Warning: --output ignored during REPL session"

  configDir <- getXdgDirectory XdgConfig "panini"
  createDirectoryIfMissing True configDir
  let historyFile = configDir </> "repl_history"
  let replConf = replSettings (Just historyFile)

  traceFile <- whenMaybe panOpts.traceToFile (openLogFileFor "repl")

  let eventHandler ev0 = do
        ev <- updatePV addSourceLines ev0
        whenJust traceFile (putEventFile panOpts ev)
        when (panOpts.trace || isErrorEvent ev) (putEventStderr panOpts ev)
        -- TODO: consider logging with getExternalPrint instead of to stderr

  let panState0 = defaultState 
        { eventHandler
        , Panini.Monad.smtTimeout = panOpts.smtTimeout 
        }

  void $ runPan panState0 $ runInputT replConf $ do
    lift smtInit
    repl panOpts

  whenJust traceFile hClose
  
  exitSuccess

-- | Panini REPL.
repl :: PanOptions -> InputT Pan ()
repl panOpts = outputStrLn banner >> loop
  where
    banner = version ++ "\nType :help for more information."
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

    handleInput = cancellable . \case
      Nothing -> outputStrLn byeMsg
      Just "" -> loop
      Just (':':(splitCmd -> (cmd,args)))
        | cmd `isPrefixOf` "quit"  -> handleInput Nothing
        | cmd `isPrefixOf` "paste" -> outputStrLn multiMsg >> loopMultiline []
        | Just f <- lookup cmd commandPrefixes -> f args >> loop
        | otherwise -> outputStrLn ("unknown command :" ++ cmd) >> loop
      Just input -> evaluateInput panOpts input >> loop

    cancellable = 
      handleInterrupt (outputStrLn "Cancelled." >> loop) . withInterrupt

    splitCmd = bimap (map toLower) (dropWhile isSpace) . break isSpace
    
    commandPrefixes = map head $ groupBy ((==) `on` fst) $ sortOn fst $ concat
                    $ map (uncurry zip . bimap (tail . inits) repeat) 
                    $ commands panOpts
    
-------------------------------------------------------------------------------

commands :: PanOptions -> [(String, String -> InputT Pan ())]
commands panOpts = 
  [ ("help", const help)
  , ("load", loadFiles panOpts . words)
  , ("show", showEnv panOpts)
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

loadFiles :: PanOptions -> [String] -> InputT Pan ()
loadFiles panOpts = mapM_ (loadFile panOpts)

loadFile :: PanOptions -> FilePath -> InputT Pan ()
loadFile panOpts f = lift $ continueOnError $ do
  logMessage $ "Read" <+> pretty f
  src <- tryIO NoPV $ Text.readFile f
  let ext = takeExtension f
  let loadFunc | panOpts.pythonInput || ext == ".py" = loadModulePython
               | otherwise                           = loadModule
  (module_, prog) <- loadFunc src f
  elaborate module_ prog
  -- TODO: output summary like "Ok, 23 modules loaded."

showEnv :: PanOptions -> String -> InputT Pan ()
showEnv panOpts "modules" = do
  env <- lift get
  liftIO $ putDocStdout panOpts $ prettyList env.loadedModules <> "\n"
showEnv panOpts _ = do
  env <- lift $ gets environment
  let doc = vsep $ map prettyDef $ sortedDefinitions env
  liftIO $ putDocStdout panOpts (doc <> "\n")
 where
  prettyDef = \case
    Assumed{_name,_type} -> 
      "⊢" <+> pretty _name <+> colon <+> pretty _type
    Rejected{_name,_error} -> hang 2 $ 
      "↯" <+> pretty _name <\> pretty _error
    Inferred{_name,_inferredType} -> 
      "⊢" <+> pretty _name <+> colon <+> pretty _inferredType
    Invalid{_name,_inferredType,_error} -> hang 2 $ 
      "↯" <+> pretty _name <+> colon <+> pretty _inferredType <\> pretty _error
    Verified{_name,_solvedType} -> 
      "⊨" <+> pretty _name <+> colon <+> pretty _solvedType
    Unverified{_name,_solvedType,_reason} -> hang 2 $
      "?" <+> pretty _name <+> colon <+> pretty _solvedType <\> pretty _reason

evaluateInput :: PanOptions -> String -> InputT Pan ()
evaluateInput panOpts input = lift $ continueOnError $ do
  let src = Text.pack input
  let loadFunc | panOpts.pythonInput = loadModulePython
               | otherwise           = loadModule
  (module_, prog) <- loadFunc src "<repl>"
  elaborate module_ prog

-------------------------------------------------------------------------------

-- | Panini REPL settings with working autocomplete.
replSettings :: Maybe FilePath -> Settings Pan
replSettings histFile =
  Settings
    { complete = autocomplete,
      historyFile = histFile,
      autoAddHistory = True
    }

autocomplete :: CompletionFunc Pan
autocomplete = (sorted <$>) . fallbackCompletion completeCommands completeFiles
  where
    completeCommands = completeWord' Nothing isSpace $ \case
      [] -> return []
      s  -> return $ map simpleCompletion $ filter (s `isPrefixOf`) cmds
    
    cmds = map (':':) $ ["quit", "paste"] ++ map fst (commands undefined)

    completeFiles (r,s) = case splitCmd (reverse r) of
      (":load", _) -> completeFilename (r,s)
      _            -> pure (r, [])
    
    splitCmd = bimap (map toLower) (dropWhile isSpace) . break isSpace

    sorted (r,cs) = (r, List.sortBy (compare `on` display) cs)
