module Panini.Modules
  ( Module
  , moduleName
  , modulePath
  , stdinModule
  , replModule
  , findModule
  ) where

import Data.Text (Text)
import Data.Text qualified as Text
import Prelude
import System.Directory
import System.FilePath
import Panini.Pretty.Printer

-------------------------------------------------------------------------------

data Module = Module
  { _name :: Text
  , _path :: Maybe FilePath  -- relative to cwd
  }
  deriving stock (Show, Eq, Ord)

instance Pretty Module where
  pretty Module{_name} = pretty _name

-- | Canonical name of the module.
moduleName :: Module -> Text
moduleName Module{_name} = _name

-- | Location of the module relative to the working directory. If 'Nothing',
-- then the module is ephemeral (i.e., loaded from stdin or the REPL).
modulePath :: Module -> Maybe FilePath
modulePath Module{_path} = _path

stdinModule :: Module
stdinModule = Module "<stdin>" Nothing

replModule :: Module
replModule = Module "<repl>" Nothing

-- | @findModule path otherModule searchDirs@ tries to find the module located
-- at @path@ using the following procedure:
--
--    1. If @path@ is absolute, try it directly.
--    2. If @otherModule@ is given, try a path relative to that module.
--    3. If there is no @otherModule@, try a path relative to the CWD.
--    4. Try a path relative to each directory in @searchDirs@, in order.
-- 
-- The given @path@ can be a proper file path, but may also lack an extension,
-- in which case @.pan@ is assumed. At each step of the procedure, the function
-- checks if a file exists at that location. It does /not/ try to read the
-- contents of the file or check if it is indeed a valid Panini file.
--
-- Returns 'Nothing' if the module cannot be located.
--
findModule :: FilePath -> Maybe Module -> [FilePath] -> IO (Maybe Module)
findModule path0 otherModule searchDirs = tryAbsolute
  where
    path = if hasExtension path0 then path0 else path0 <.> "pan"

    tryAbsolute = case isAbsolute path of
      True -> doesFileExist path >>= \case
        True -> returnModuleWith path
        False -> return Nothing
      False -> tryOtherModule
    
    tryOtherModule = case otherModule of
      Just (Module _ (Just otherPath)) -> do
        let path' = takeDirectory otherPath </> path
        doesFileExist path' >>= \case
          True -> returnModuleWith path'
          False -> trySearchDirs searchDirs
      _ -> tryCWD
    
    tryCWD = do
      path' <- makeRelativeToCurrentDirectory path
      doesFileExist path' >>= \case
        True -> returnModuleWith path'
        False -> trySearchDirs searchDirs
    
    trySearchDirs (dir:dirs) = do
      let path' = dir </> path
      doesFileExist path' >>= \case
        True -> returnModuleWith path'
        False -> trySearchDirs dirs

    trySearchDirs [] = return Nothing

    returnModuleWith p = do
      relPath <- makeRelativeToCurrentDirectory p
      let _name = Text.pack $ dropExtension relPath
      return $ Just $ Module {_name, _path = Just relPath }
