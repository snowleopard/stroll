{-# LANGUAGE OverloadedStrings, RecordWildCards, ScopedTypeVariables #-}

module Development.Stroll where

import Control.Monad
import Data.Yaml
import Development.Shake hiding (doesFileExist)
import Development.Shake.FilePath
import Development.Stroll.Hash
import Development.Stroll.Script
import Development.Stroll.Trace
import System.Directory

import qualified Data.ByteString as B

getScripts :: FilePath -> IO [Script]
getScripts dir = getDirectoryFilesIO "" [dir <//> "*"]

stroll :: FilePath -> IO ()
stroll dir = do
    scripts <- getScripts dir
    todo    <- filterM outOfDate scripts
    case todo of
        (script:_) -> do
            putStrLn ("Executing " ++ script)
            void (execute script)
            stroll dir
        _ -> putStrLn "Done"

outOfDate :: Script -> IO Bool
outOfDate script = do
    let stroll = script <.> "stroll"
    exists <- doesFileExist stroll
    if exists
    then do
        trace <- B.readFile stroll
        case decodeEither' trace of
            Left err -> error (show err)
            Right t  -> not <$> upToDate t hashFile
    else return False
