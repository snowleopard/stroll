{-# LANGUAGE OverloadedStrings, RecordWildCards, ScopedTypeVariables #-}

module Development.Stroll.Trace where

import Development.Stroll.Base
import Development.Stroll.Hash

import Data.ByteString (ByteString)
import Data.Text (pack)
import Data.Map (Map)
import Data.Yaml
import System.Exit

-- See [Note: Operations]
data Operation = Read  (Maybe Hash)
               | Write (Maybe Hash)

instance ToJSON Operation where
    toJSON (Read  contents) = object ["read"  .= toJSON (toText <$> contents)]
    toJSON (Write contents) = object ["write" .= toJSON (toText <$> contents)]

type Operations = Map Key Operation

{- [Note: Operations]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Stroll records file-system operations performed while executing a script.

The current model allows only two basic operations for the sake of simplicity.

* 'Read' corresponds to reading a 'Key'. Reading the same 'Key' multiple times
  is allowed, and we assume that all such reads yield the same value, whose
  'Hash' is recorded. We record 'Nothing' if the 'Key' does not exist.

* 'Write' corresponds to writing a 'Key'. Writing the same 'Key' multiple times
  is allowed, and we record the 'Hash' of the latest written value. If we both
  read and write a 'Key', we consider this a 'Write' operation. We record
  'Nothing' if the script /deletes/ the 'Key': typical examples are temporary
  files, as well as build artefacts deleted by a "cleaning" script.

-}

-- After a 'Script' is executed, we record the following trace
data Trace = Trace { scriptHash :: Hash
                   , exitCode   :: ExitCode
                   , operations :: Operations
                   , stdout     :: ByteString
                   , stderr     :: ByteString }

instance ToJSON Trace where
    toJSON Trace{..} = object
        [ "script-hash" .= toText scriptHash
        , "exit-code"   .= pack (show exitCode)
        , "operations"  .= operations ]
