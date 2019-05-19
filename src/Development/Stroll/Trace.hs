{-# LANGUAGE OverloadedStrings, RecordWildCards, ScopedTypeVariables #-}

module Development.Stroll.Trace where

import Development.Stroll.Base
import Development.Stroll.Hash

import Control.Selective (allS)
import Data.Map (Map)
import Data.Text (Text, pack, unpack)
import Data.Yaml
import System.Exit
import Text.Read

import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map            as Map

{-| Stroll records file-system operations performed while executing a script.

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
data Operation = Read  (Maybe Hash)
               | Write (Maybe Hash)
    deriving (Eq, Show)

instance ToJSON Operation where
    toJSON (Read  contents) = object ["read"  .= toJSON (toText <$> contents)]
    toJSON (Write contents) = object ["write" .= toJSON (toText <$> contents)]

instance FromJSON Operation where
    parseJSON = withObject "Operation" $ \o ->
        if HashMap.size o /= 1
        then fail "Exactly one operation expected"
        else case HashMap.lookup "read" o of
            Just value -> Read <$> parseJSON value
            Nothing    -> case HashMap.lookup "write" o of
                Just value -> Write <$> parseJSON value
                Nothing    -> fail "Unknown operation"

-- | A 'Trace' is recorded after executing a build 'Script'. Thanks to the
-- 'FromJSON' and 'ToJSON' instances, you can easily serialise and deserialise
-- traces. For example, see 'encodeFile' and 'decodeFileEither' for storing
-- traces in YAML files.
data Trace = Trace { scriptPath :: FilePath
                   , scriptHash :: Hash
                   , exitCode   :: ExitCode
                   , operations :: Map Key Operation }
    deriving Show

instance ToJSON Trace where
    toJSON Trace{..} = object
        [ "script-path" .= pack scriptPath
        , "script-hash" .= scriptHash
        , "exit-code"   .= pack (show exitCode)
        , "operations"  .= operations ]

instance FromJSON Trace where
    parseJSON = withObject "Trace" $ \o -> Trace
        <$>  o .: "script-path"
        <*>  o .: "script-hash"
        <*> (o .: "exit-code" >>= parseExitCode)
        <*>  o .: "operations"
      where
        parseExitCode :: Text -> Parser ExitCode
        parseExitCode text = case readMaybe (unpack text) of
            Nothing  -> fail ("Cannot parse exit code " ++ show text)
            Just res -> return res

-- | Given a build 'Trace', and a function to compute the 'Hash' of a key's
-- value (or 'Nothing' if the key does not exist), return 'True' if the trace is
-- /up-to-date/, that is:
--
-- * The build script that was used to produce the trace is unchanged.
--
-- * All keys recorded in the trace have expected values.
upToDate :: Trace -> (Key -> IO (Maybe Hash)) -> IO Bool
upToDate Trace{..} fetchHash =
    allS match ((scriptPath, Read (Just scriptHash)) : Map.toList operations)
  where
    match :: (Key, Operation) -> IO Bool
    match (key, operation) = (== value) <$> fetchHash key
      where
        value = case operation of { Read v -> v; Write v -> v }
