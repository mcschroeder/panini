module Panini.Provenance
  ( SrcLoc(..)
  , PV(..)
  , HasProvenance(..)
  , updatePV
  , addSourceLines
  , readSrcLocLines
  ) where

import Control.Monad
import Data.ByteString qualified as BS
import Data.Text (Text)
import Data.Text.Encoding qualified as Text
import Panini.Pretty
import Prelude
import System.IO

-------------------------------------------------------------------------------

-- | A location in a source file.
data SrcLoc = SrcLoc
  { file  :: FilePath    -- ^ source file
  , begin :: (Int, Int)  -- ^ (line, column)
  , end   :: (Int, Int)  -- ^ (line, column)
  }
  deriving stock (Eq, Ord, Show, Read)

instance Pretty SrcLoc where
  pretty (SrcLoc f (l1, c1) (l2, c2))
    | l1 == l2, c1 == c2 = flc
    | l1 == l2           = flc <> "-" <> pretty c2
    | otherwise          = flc <> "-" <> pretty l2 <> ":" <> pretty c2
    where
      flc = pretty f <> ":" <> pretty l1 <> ":" <> pretty c1

-- | Provenance information: where something comes from.
data PV
  = FromSource SrcLoc (Maybe Text) 
    -- ^ Location in source file, with optional source lines.
  | Derived PV String
    -- ^ Derivation from original (e.g., type synthesis or variable renaming).
  | NoPV 
    -- ^ Most likely machine-generated data.
  deriving stock (Eq, Show, Read)

-- | Ordering by (original) source location.
instance Ord PV where
  compare (Derived pv1 _) pv2 = compare pv1 pv2
  compare pv1 (Derived pv2 _) = compare pv1 pv2
  compare (FromSource loc1 _) (FromSource loc2 _) = compare loc1 loc2
  compare (FromSource _ _) NoPV = GT
  compare NoPV (FromSource _ _) = LT
  compare NoPV NoPV = EQ

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
readNumLines n0 h = go [] n0
 where
  go s 0 = return $ Text.decodeUtf8 $ mconcat $ reverse s
  go s n = hIsEOF h >>= \case
    True  -> go s 0
    False -> do t <- BS.hGetLine h
                go (t:s) (n-1)
