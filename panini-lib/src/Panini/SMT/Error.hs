{-# LANGUAGE RecordWildCards #-}
module Panini.SMT.Error where

import Prelude
import Panini.Diagnostic
import Panini.Pretty

data Error 
  = InitError
      { _exitCode :: Maybe Int
      , _output   :: String 
      }
  | QueryError
      { _args     :: [String]
      , _query    :: String
      , _exitCode :: Maybe Int
      , _output   :: String
      }

instance Diagnostic Error where
  diagnosticMessage = \case
    InitError{..} -> bulletpoints [errMsg]
     where
      errMsg =
        "Error initializing SMT solver" <+> 
        parens ("exit code" <+> maybe "n/a" pretty _exitCode) <> colon <\>
        pretty _output
    QueryError{..} -> bulletpoints [errMsg, queryMsg]
     where
      errMsg = 
        "Unexpected SMT solver output" <+>
        parens ("exit code" <+> maybe "n/a" pretty _exitCode) <> colon <\>
        pretty _output
      queryMsg =
        "In response to the following query:" <\>
        pretty _query

