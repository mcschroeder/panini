{-# LANGUAGE RecordWildCards #-}
module Panini.SMT.Event where

import Prelude
import Panini.Diagnostic
import Panini.Pretty

data Event 
  = Init { _version  :: String }
  | InitError
      { _exitCode :: Maybe Int
      , _output   :: String 
      }
  | Query
      { _args     :: [String]
      , _query    :: String
      }
  | QueryResult
      { _args     :: [String]
      , _query    :: String
      , _output   :: String
      }
  | QueryError
      { _args     :: [String]
      , _query    :: String
      , _exitCode :: Maybe Int
      , _output   :: String
      }

instance Diagnostic Event where
  diagnosticMessage = \case
    Init{..} -> pretty _version
    InitError{..} -> bulletpoints [errMsg]
     where
      errMsg =
        "Error initializing SMT solver" <+> 
        parens ("exit code" <+> maybe "n/a" pretty _exitCode) <> colon <\>
        pretty _output

    Query{..} -> pretty _query
    QueryResult{..} -> pretty _output
    QueryError{..} -> bulletpoints [errMsg, queryMsg]
     where
      errMsg = 
        "Unexpected SMT solver output" <+>
        parens ("exit code" <+> maybe "n/a" pretty _exitCode) <> colon <\>
        pretty _output
      queryMsg =
        "In response to the following query:" <\>
        pretty _query

