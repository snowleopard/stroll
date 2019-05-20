{-# LANGUAGE OverloadedStrings, RecordWildCards, ScopedTypeVariables #-}
module Development.Stroll.Script (Script, execute) where

import Control.Selective
import Control.Monad
import Data.ByteString (ByteString)
import Data.List
import Data.Map (Map)
import Data.Yaml
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Stroll.Hash
import Development.Stroll.Trace
import System.Directory

import qualified Data.ByteString as B
import qualified Data.Map        as Map

type Script = FilePath

-- | Execute a specified script, return the resulting 'Trace', and update
-- persistent information files:
--
-- * Write the trace to the @.stroll@ file.
-- * If there is any standard output, write it to the @.stdout@ file.
-- * If there is any standard error output, write it to the @.stderr@ file.
--
-- Fails with an error if the given script does not exist.
execute :: Script -> IO Trace
execute script = do
    exists <- doesFileExist script
    unless exists $ error ("Script not found: " ++ script)
    (Exit code, StdoutTrim out, Stderr err, fsatraces) <- cmd Shell script
    cwd <- getCurrentDirectory
    ops <- Map.traverseWithKey updateHash (decodeFSATraces cwd fsatraces)
    let trace = Trace code ops
    B.writeFile (script <.> "stroll") (encode trace)
    updateFile (script <.> "stdout") out
    updateFile (script <.> "stderr") err
    return trace
  where
    updateHash :: FilePath -> Operation -> IO Operation
    updateHash f (Read  _) = Read  <$> hashFile f
    updateHash f (Write _) = Write <$> hashFile f
    updateFile :: FilePath -> ByteString -> IO ()
    updateFile file contents
        | B.null contents = whenS (doesFileExist file) (removeFile file)
        | otherwise       = B.writeFile file contents

decodeFSATraces :: FilePath -> [FSATrace] -> Map FilePath Operation
decodeFSATraces dir = go Map.empty
  where
    go res []     = res
    go res (t:ts) = case t of
        FSARead   f -> add Read  f
        FSAQuery  f -> add Read  f
        FSAWrite  f -> add Write f
        FSADelete f -> add Write f
        FSATouch  f -> add Write f
        FSAMove d s -> error ("Moving files not supported: " ++ s ++ " => " ++ d)
      where
        add :: (Maybe Hash -> Operation) -> FilePath -> Map FilePath Operation
        add c file = case relativise dir file of
            Nothing   -> go res ts -- skip files outside the root directory
            Just path -> case Map.lookup path res of
                Just (Write _) -> go res ts -- Write's are final
                _              -> go (Map.insert path (c Nothing) res) ts

relativise :: FilePath -> FilePath -> Maybe FilePath
relativise dir file = dropWhile isPathSeparator <$> stripPrefix dir file
