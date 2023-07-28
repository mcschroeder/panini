module Panini.CLI.REPL
  ( repl,
    replSettings,
  )
where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Strict
import Data.Bifunctor
import Data.Char (isSpace, toLower)
import Data.Function
import Data.List (isPrefixOf, groupBy, sortOn, inits)
import Data.Map qualified as Map
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Panini.Error
import Panini.Language.AST
import Panini.Language.Elaborator
import Panini.Language.Elaborator.Environment
import Panini.Language.Infer
import Panini.Logger
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
    

commands :: [(String, String -> InputT Pan ())]
commands = 
  [ ("format", formatInput)
  , ("type", synthesizeType)
  , ("load", loadFiles . words)
  , ("show", const showState)
  , ("forget", forgetVars . words)
  , ("grammar", solveGrammarFile . head . words)
  ]

solveGrammarFile :: FilePath -> InputT Pan ()
solveGrammarFile f = do
  src <- liftIO $ Text.readFile f
  case parseConstraint f src of
    Left err -> outputPretty err
    Right _c -> do
      undefined
      -- outputPretty c
      -- let c' = Panini.Solver.Grammar.solve c
      -- outputPretty c'

formatInput :: String -> InputT Pan ()
formatInput input = do
  case parseTerm "<repl>" $ Text.pack input of
    Left err -> do
      err' <- liftIO $ updatePV (addSourceLinesREPL input) err
      outputPretty err'
    Right e -> outputPretty e

synthesizeType :: String -> InputT Pan ()
synthesizeType input = do
  case parseTerm "<repl>" $ Text.pack input of
    Left err -> do
      err' <- liftIO $ updatePV (addSourceLinesREPL input) err
      outputPretty err'
    Right tm -> do
      g <- lift $ envToContext <$> gets environment
      err2 <- lift $ tryError $ infer g tm
      case err2 of
        Left err -> do
          err' <- liftIO $ updatePV (addSourceLinesREPL input) err
          outputPretty err'        
        Right (tm',t,vc) -> do
          outputPretty $ "⊢ " <> pretty tm'
          outputPretty $ "↗ " <> pretty t
          outputPretty $ "⫤ " <> pretty vc --(simplifyCon vc)
  
evaluateInput :: String -> InputT Pan ()
evaluateInput input = do
  res <- lift $ tryError $ elaborateProgram =<< lift (except $ parseInput input) 
  case res of
    Left err -> do
      err' <- liftIO $ updatePV (addSourceLinesREPL input) err
      outputPretty err'
    Right () -> return ()

loadFiles :: [FilePath] -> InputT Pan ()
loadFiles fs = forM_ fs $ \f -> do
  lift $ logMessage "REPL" $ "Read " ++ f
  src <- liftIO $ Text.readFile f
  case parseProgram f src of
    Left err1 -> outputPretty err1
    Right prog -> do
      res <- lift $ tryError $ elaborateProgram prog
      case res of
        Left err2 -> do
          err2' <- liftIO $ updatePV addSourceLines err2
          outputPretty err2'
        Right () -> do          
          --showState
          return ()

showState :: InputT Pan ()
showState = do
  PanState{environment} <- lift get
  forM_ (Map.toAscList environment) $ \(_,def) -> case def of
    Assumed{_name,_givenType} ->
      outputStrLn $ "✳️  " ++ showPretty _name ++ " : " ++ showPretty _givenType
    Rejected{_name,_givenType,_typeError} ->
      outputStrLn $ "❗ " ++ showPretty _name ++ " : " ++ showPretty _givenType ++ "  [type error]"
    Inferred{_name,_inferredType,_vc} ->
      outputStrLn $ "❓ " ++ showPretty _name ++ " : " ++ showPretty _inferredType
    Invalid{_name,_inferredType,_vc} ->
      outputStrLn $ "‼️  " ++ showPretty _name ++ " : " ++ showPretty _inferredType ++ "  [solver error]"
    Verified{_name,_inferredType,_vc,_solution} ->
      outputStrLn $ "✅ " ++ showPretty _name ++ " : " ++ showPretty _inferredType




-- prettyEnv :: Environment -> Doc Ann
-- prettyEnv env = forM_ (Map.toList env) $ \(x,def) -> case def of
--   Assumed {_givenType} -> "" <+> x <+> sym ":" <+> pretty _givenType
--   Rejected {_givenType,_typeError} -> "" <+> x <+> sym ":" <+> pretty _givenType


  -- ElabState{pan_types, pan_terms, pan_vcs} <- lift get
  -- outputStrLn "\ntyping context"
  -- mapM_ outputPretty $ Map.toList pan_types
  -- outputStrLn "\nterm context"
  -- mapM_ outputPretty $ Map.toList pan_terms
  -- outputStrLn "\nverification conditions"
  -- -- mapM_ outputPretty $ Map.toList pan_vcs
  -- forM_ (Map.toList pan_vcs) $ \(x,vc) -> do
  --   outputPretty x
  --   outputPretty vc
  --   outputStrLn "---"
  --   r <- liftIO $ sat vc []
  --   case r of
  --     True -> outputStrLn "SAT"
  --     False -> outputStrLn "UNSAT"
  --   -- r <- liftIO $ Panini.SMT.solve vc []
  --   -- case r of
  --   --   Nothing -> outputStrLn "UNSAT"
  --   --   Just s -> do
  --   --     outputStrLn "SAT"
  --   --     forM_ (Map.toList s) $ \(k,(xs,p)) -> do
  --   --       outputPretty $ (PRel Eq (PHorn k (map V xs)) p)

forgetVars :: [String] -> InputT Pan ()
forgetVars _xs = undefined --do
  -- forM_ xs $ \x -> do
  --   let n = Name (Text.pack x) NoPV
  --   lift $ modify' $ \s -> s { pan_types = Map.delete n s.pan_types }
  --   lift $ modify' $ \s -> s { pan_types = Map.delete n s.pan_types }
  --   lift $ modify' $ \s -> s { pan_types = Map.delete n s.pan_types }


-- TODO: error source /= var name source (previous vs current definition)

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

class Inputable a where
  parseInput :: String -> Either Error a

instance Inputable (Term Untyped) where
  parseInput = parseTerm "<repl>" . Text.pack

instance Inputable Statement where
  parseInput = parseStatement "<repl>" . Text.pack

instance Inputable Program where
  parseInput = parseProgram "<repl>" . Text.pack

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
