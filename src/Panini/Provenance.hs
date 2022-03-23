module Panini.Provenance
  ( SrcLoc(..)
  , PV(..)
  , HasProvenance(..)
  , updatePV
  , addSourceLines
  , readSrcLocLines
  ) where

import Data.ByteString qualified as BS
import Data.Text (Text)
import Data.Text.Encoding qualified as Text
import Prelude
import System.IO
import Control.Monad

-------------------------------------------------------------------------------

-- | A location in a source file.
data SrcLoc = SrcLoc
  { file  :: FilePath    -- ^ source file
  , begin :: (Int, Int)  -- ^ (line, column)
  , end   :: (Int, Int)  -- ^ (line, column)
  }
  deriving stock (Eq, Ord, Show, Read)

-- | Provenance information: where something comes from.
data PV
  = FromSource SrcLoc (Maybe Text) 
    -- ^ Location in source file, with optional source lines.
  | Derived PV String
    -- ^ Derivation from original (e.g., type synthesis or variable renaming).
  | NoPV 
    -- ^ Most likely machine-generated data.
  deriving stock (Show, Read)

-- | Types with provenance information.
class HasProvenance a where
  getPV :: a -> PV
  setPV :: PV -> a -> a

updatePV :: (HasProvenance a, Monad m) => (PV -> m PV) -> a -> m a
updatePV f a = do
  let pv = getPV a
  pv' <- f pv
  return $ setPV pv' a

-------------------------------------------------------------------------------

-- | Adds source lines to provenance information, if possible.
addSourceLines :: PV -> IO PV
addSourceLines (FromSource loc Nothing) = 
  FromSource loc . Just <$> readSrcLocLines loc
addSourceLines (Derived pv x) = do
  pv' <- addSourceLines pv
  return $ Derived pv' x
addSourceLines x = pure x

-- | Returns the *lines* touched by the given `SrcLoc`.
readSrcLocLines :: SrcLoc -> IO Text
readSrcLocLines (SrcLoc fp (l1,_) (l2,_)) = 
  withFile fp ReadMode $ \h -> do
    seekToLine l1 h
    readNumLines (l2 - l1 + 1) h

seekToLine :: Int -> Handle -> IO ()
seekToLine n h = replicateM_ (n - 1) $ BS.hGetLine h

readNumLines :: Int -> Handle -> IO Text
readNumLines n h = do
  ls <- replicateM n $ BS.hGetLine h
  return $ Text.decodeUtf8 $ mconcat ls
