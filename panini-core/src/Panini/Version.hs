{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module Panini.Version where

import GitHash
import Prelude
import Text.Printf

version :: String
version = printf "Panini 0.1 (%s)" gitString
  where
    gitString = case $$tGitInfoCwdTry of
      Left _ -> "???"
      Right gi
        | giDirty gi -> giDescribe gi ++ "-dirty"
        | otherwise  -> giDescribe gi
