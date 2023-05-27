{-# LANGUAGE DeriveGeneric, OverloadedStrings, RecordWildCards #-}
module Development.Stroll.Trace where

import Development.Stroll.Hash

import Control.Monad
import Data.Map (Map)
import Data.Text (Text, pack, unpack)
import Data.Yaml
import GHC.Generics
import System.Exit
import Text.Read

import qualified Data.Map as Map

{-| Stroll records file-system operations performed while executing a script.

The current model allows only two basic operations for the sake of simplicity.

* 'Read' corresponds to reading from a file. Reading from the same file multiple
  times is allowed, and we assume that all such reads yield the same value,
  whose 'Hash' is recorded. We record 'Nothing' if the file does not exist.

* 'Write' corresponds to writing to a file. Writing to the same file multiple
  times is allowed, and we record the 'Hash' of the latest written value. If we
  both read from and write to the same file, we consider this a 'Write'
  operation. We record 'Nothing' if the script /deletes/ the file: typical
  examples are temporary files, and build artefacts deleted by clean up scripts.

-}
data Operation = Read (Maybe Hash) | Write (Maybe Hash)
    deriving (Eq, Generic, Ord, Show)

-- | 'Operations' is just a map from a file to a recorded operation.
type Operations = Map FilePath Operation

instance ToJSON   Operation
instance FromJSON Operation

-- | A 'Trace' is recorded after executing a build 'Script'. Thanks to the
-- 'FromJSON' and 'ToJSON' instances, you can easily serialise and deserialise
-- traces. For example, see 'encodeFile' and 'decodeFileEither' for storing
-- traces in YAML files.
data Trace = Trace { exitCode :: ExitCode, operations :: Operations }
    deriving Show

instance ToJSON Trace where
    toJSON Trace{..} = object
        [ "exit-code"  .= pack (show exitCode)
        , "operations" .= operations ]

instance FromJSON Trace where
    parseJSON = withObject "Trace" $ \o -> Trace
        <$> (o .: "exit-code" >>= parseExitCode)
        <*>  o .: "operations"
      where
        parseExitCode :: Text -> Parser ExitCode
        parseExitCode text = case readMaybe (unpack text) of
            Nothing  -> fail ("Cannot parse exit code " ++ show text)
            Just res -> return res

-- | Given a build 'Trace', and a function to compute the 'Hash' of a file
-- contents (or 'Nothing' if the file does not exist), return 'True' if the
-- trace is /up-to-date/, that is, the contents of all files in the trace
-- matches the recorded hashes.
upToDate :: Trace -> (FilePath -> IO (Maybe Hash)) -> IO Bool
upToDate trace fetchHash = null <$> traceMisses trace fetchHash

-- | Given a build 'Trace', and a function to compute the 'Hash' of a file
-- contents (or 'Nothing' if the file does not exist), return a list of files
-- whose contents does not match the hashes recorded in the trace.
traceMisses :: Trace -> (FilePath -> IO (Maybe Hash)) -> IO [(FilePath, Operation)]
traceMisses Trace{..} fetchHash = filterM miss (Map.toList operations)
  where
    miss :: (FilePath, Operation) -> IO Bool
    miss (file, operation) = (/= value) <$> fetchHash file
      where
        value = case operation of { Read v -> v; Write v -> v }
