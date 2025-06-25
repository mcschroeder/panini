{-# LANGUAGE TemplateHaskell #-}
module Panini.Version where

import Panini.Version.Git
import Prelude
import Text.Printf

version :: String
version = printf "Panini 0.1 (%s)" ($(gitString) :: String)
