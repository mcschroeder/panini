{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Tasty
import Test.Tasty.Golden
import System.FilePath
import qualified Data.Text.IO as Text
import Data.ByteString.Lazy (ByteString)
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Except
import qualified Data.Map as Map
import Data.ByteString.Lazy.Char8 qualified as BS
import Data.List (sort)
import Data.Text.Lazy.Encoding (encodeUtf8)
import qualified Data.Text.Lazy as TL

import Panini.Monad
import Panini.Elaborator
import Panini.Elaborator.Environment
import Panini.Syntax
import Panini.Parser
import Panini.Pretty.Printer

main :: IO ()
main = defaultMain =<< grammarTests

grammarTests :: IO TestTree
grammarTests = do
  paniniSources <- sort <$> findByExtension [".pan"] "tests"
  let tests = [ goldenVsString f o (inferGrammar f) 
              | f <- paniniSources
              , let o = replaceExtension f "g"
              ]
  return $ testGroup "Grammar inference tests" tests


inferGrammar :: FilePath -> IO ByteString
inferGrammar f = do
  src <- Text.readFile f
  case parseProgram f src of
    Left err1 -> fail $ showPretty err1
    Right prog -> do
      let st0 = initState { logger = Nothing }
      res <- runExceptT $ execStateT (tryError $ elaborateProgram prog) st0
      case res of
        Left err2 -> fail $ showPretty err2
        Right st -> case Map.lookup "f" (environment st) of
          Just Verified{_solution} -> case Map.lookup (KVar 0 [TString]) _solution of
            Nothing -> fail "k0(s) not found"
            Just g -> do
              let opts = RenderOptions { styling = Nothing, unicode = True, fixedWidth = Nothing }
              let str = renderDoc opts $ pretty g
              return $ encodeUtf8 $ TL.fromStrict str
          _ -> fail "unable to find verified definition for 'f'"
