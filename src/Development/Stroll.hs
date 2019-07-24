module Development.Stroll where

import Control.Monad
import Data.Bool
import Data.Yaml
import Development.Shake hiding (doesFileExist)
import Development.Shake.FilePath
import Development.Stroll.Hash
import Development.Stroll.Script
import Development.Stroll.Trace
import System.Directory
import System.Exit

import qualified Data.ByteString as B

getScripts :: FilePath -> IO [Script]
getScripts dir = do
    files <- getDirectoryFilesIO "" [dir <//> "*"]
    return (filter notStroll files)
  where
    notStroll :: FilePath -> Bool
    notStroll f = takeExtension f `notElem` [".stroll", ".stdout", ".stderr"]

data Status = UpToDate | OutOfDate | Error deriving Eq

status :: Script -> IO Status
status script = do
    let stroll = script <.> "stroll"
    exists <- doesFileExist stroll
    if not exists then return OutOfDate else do
        trace <- B.readFile stroll
        case decodeEither' trace of
            Left err -> error (show err) -- Maybe return 'OutOfDate'?
            Right t  -> bool OutOfDate result <$> upToDate t hashFile
              where
                result = if exitCode t == ExitSuccess then UpToDate else Error

stroll :: FilePath -> IO ()
stroll dir = do
    scripts   <- getScripts dir
    statuses  <- sequence [ (s,) <$> status s | s <- scripts ]
    let outOfDate = filter ((==OutOfDate) . snd) statuses
    case outOfDate of
        ((script,_):_) -> do
            putStrLn ("Executing " ++ script ++ "...")
            void (execute script)
            stroll dir
        _ -> do
            let failed = filter ((==Error) . snd) statuses
            forM_ failed $ \(script,_) ->
                putStrLn ("Script " ++ script ++ " has failed.")
            putStrLn "Done"
