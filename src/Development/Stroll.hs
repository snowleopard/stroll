{-# LANGUAGE OverloadedStrings, RecordWildCards, ScopedTypeVariables #-}

module Development.Stroll where

import Crypto.Hash
import Data.ByteArray.Encoding
import Data.ByteString (ByteString)
import Data.Text (Text, pack)
import Data.Text.Encoding
import Data.Map (Map)
import Data.Yaml
import Development.Shake.Command
import System.Exit
import System.FilePath

import qualified Data.Set as Set

-- Vocabulary of build systems
type Key    = FilePath
type Value  = ByteString
type Hash   = Digest SHA256
type Script = FilePath

hashToText :: Hash -> Text
hashToText = decodeUtf8 . convertToBase Base16

-- See [Note: Operations]
data Operation = Read   Hash
               | Write  Hash
               | Modify Hash
               | Delete

instance ToJSON Operation where
    toJSON (Read   h) = object ["read"   .= hashToText h]
    toJSON (Write  h) = object ["write"  .= hashToText h]
    toJSON (Modify h) = object ["modify" .= hashToText h]
    toJSON Delete     = object ["delete" .= Null        ]

type Operations = Map Key Operation

{- [Note: Operations]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Stroll records all file-system operations performed while executing a script.

The current model allows only four operations for the sake of simplicity.

* 'Read' corresponds to reading a 'Key'. Reading the same 'Key' multiple times
  is allowed, and we assume that all such reads yield the same value, whose
  'Hash' is recorded.

* 'Write' corresponds to writing a 'Key'. Writing the same 'Key' multiple times
  is allowed, and we record the 'Hash' of the latest written value.

* 'Modify' corresponds to reading and writing the same 'Key', possibly multiple
  times. We record the 'Hash' of the latest written value.

* 'Delete' corresponds to deleting a 'Key', and may be combined with reading
  and/or writing the same 'Key'. Typical examples are temporary files, as well
  as build artefacts deleted by a "cleaning" script.

-}

-- After a 'Script' is executed, we record the following trace
data Trace = Trace { scriptHash :: Hash
                   , exitCode   :: ExitCode
                   , operations :: Operations
                   , stdout     :: ByteString
                   , stderr     :: ByteString }

instance ToJSON Trace where
    toJSON Trace{..} = object
        [ "script-hash" .= hashToText scriptHash
        , "exit-code"   .= pack (show exitCode)
        , "operations"  .= operations ]

-- | Run a specified script, updating the corresponding @.stroll@ file.
run :: FilePath -> Script -> IO ()
run dir script = do
    (Exit code, Stdout out, Stderr err, fsatraces :: [FSATrace]) <- cmd (Cwd dir) script
    let stroll = [ "Exit code: " ++ show code
                 , "Stdout   : " ++ show (out :: String)
                 , "Stderr   : " ++ show (err :: String)
                 , "FSAtraces: " ++ show fsatraces
                 , "Set size : " ++ show (Set.size $ Set.fromList fsatraces) ]
    writeFile (dir </> script <.> "stroll") (unlines stroll)
