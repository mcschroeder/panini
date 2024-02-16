module Panini.CLI.Common where

import Control.Monad.Extra
import Data.Text.IO qualified as Text
import Panini.CLI.Options
import Panini.Events
import Panini.Pretty as PP
import Prelude
import System.FilePath
import System.IO

-------------------------------------------------------------------------------

openLogFileFor :: FilePath -> IO Handle
openLogFileFor f = do
  h <- openFile (f -<.> "log") WriteMode
  hSetBuffering h NoBuffering
  return h

putEventFile :: PanOptions -> Event -> Handle -> IO ()
putEventFile o e = putDocFile o $ prettyEvent e <> "\n"

putEventStderr :: PanOptions -> Event -> IO ()
putEventStderr o e = putDocStderr o $ prettyEvent e <> "\n"

-------------------------------------------------------------------------------

putDocFile :: PanOptions -> Doc -> Handle -> IO ()
putDocFile o d h = hPutDoc (fileRenderOptions o) d h

putDocStderr :: PanOptions -> Doc -> IO ()
putDocStderr o d = hPutDoc (termRenderOptions o) d stderr

putDocStdout :: PanOptions -> Doc -> IO ()
putDocStdout o d = hPutDoc (termRenderOptions o) d stdout

hPutDoc :: RenderOptions -> Doc -> Handle -> IO ()
hPutDoc o d h = Text.hPutStr h $ renderDoc o d

-------------------------------------------------------------------------------

fileRenderOptions :: PanOptions -> RenderOptions
fileRenderOptions o = RenderOptions 
  { styling = Nothing
  , PP.unicode = o.unicode
  , fixedWidth = Nothing 
  }

termRenderOptions :: PanOptions -> RenderOptions
termRenderOptions o = RenderOptions
  { styling = pureIf o.color defaultStyling
  , PP.unicode = o.unicode
  , fixedWidth = o.termWidth
  }
