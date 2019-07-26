module Development.Stroll (stroll, graph, info, step, reset) where

import Algebra.Graph
import Algebra.Graph.Export.Dot
import Algebra.Graph.ToGraph
import Control.Monad
import Data.Bool
import Data.Either
import Data.Yaml
import Development.Shake hiding (doesFileExist)
import Development.Shake.FilePath
import Development.Stroll.Hash
import Development.Stroll.Script
import Development.Stroll.Trace
import System.Directory
import System.Exit
import System.IO

import qualified Data.ByteString as B
import qualified Data.Map        as Map
import qualified Data.Set        as Set

getScripts :: FilePath -> IO [Script]
getScripts dir = do
    files <- getDirectoryFilesIO "" [dir <//> "*"]
    return (filter notStroll files)
  where
    notStroll :: FilePath -> Bool
    notStroll f = takeExtension f `notElem` [".stroll", ".stdout", ".stderr"]

data Status = UpToDate | OutOfDate | Error deriving (Eq, Ord, Show)

prettyStatus :: Status -> String
prettyStatus UpToDate  = "[ up-to-date]"
prettyStatus OutOfDate = "[out-of-date]"
prettyStatus Error     = "[   error   ]"

getTrace :: Script -> IO (Maybe Trace)
getTrace script = do
    let stroll = script <.> "stroll"
    exists <- doesFileExist stroll
    if not exists then return Nothing else do
        trace <- B.readFile stroll
        return $ case decodeEither' trace of
            Left  _ -> Nothing
            Right t -> Just t

getStatus :: Script -> IO Status
getStatus script = do
    let stroll = script <.> "stroll"
    exists <- doesFileExist stroll
    if not exists then return OutOfDate else do
        trace <- B.readFile stroll
        case decodeEither' trace of
            Left err -> error (show err) -- Maybe return 'OutOfDate'?
            Right t  -> bool OutOfDate result <$> upToDate t hashFile
              where
                result = if exitCode t == ExitSuccess then UpToDate else Error

step :: FilePath -> IO ()
step script = do
    putStrLn ("Executing " ++ toStandard script ++ "...")
    hFlush stdout
    void (execute script)

stroll :: FilePath -> IO ()
stroll dir = do
    scripts  <- getScripts dir
    statuses <- sequence [ (s,) <$> getStatus s | s <- scripts ]
    let outOfDate = filter ((==OutOfDate) . snd) statuses
    case outOfDate of
        ((script,_):_) -> step script >> stroll dir
        _ -> do
            let failed = filter ((==Error) . snd) statuses
            forM_ failed $ \(script,_) ->
                putStrLn ("Script " ++ toStandard script ++ " has failed.")
            putStrLn "Done"

info :: FilePath -> IO ()
info dir = do
    scripts <- getScripts dir
    forM_ scripts $ \script -> do
        status <- getStatus script
        putStrLn $ prettyStatus status ++ " " ++ script

type DependencyGraph = Graph (Either FilePath Script)

-- | Build a dependency graph using the information available in @.stroll@ files.
dependencyGraph :: FilePath -> IO DependencyGraph
dependencyGraph dir = do
    scripts <- getScripts dir
    parts <- forM scripts $ \script -> do
        let stroll = script <.> "stroll"
        exists <- doesFileExist stroll
        if not exists then return (vertex $ Right script) else do
            trace <- B.readFile stroll
            return $ case decodeEither' trace of
                Left err -> error (show err) -- Maybe return a vertex?
                Right t  -> edges . map toEdge . Map.toList $ operations t
                  where
                    toEdge (file, Read  _) = (Left file, Right script)
                    toEdge (file, Write _) = (Right script, Left file)
    return (overlays parts)

graph :: FilePath -> IO ()
graph dir = do
    scripts  <- getScripts dir
    statuses <- sequence [      (Right s,) <$> getStatus s | s <- scripts ]
    traces   <- sequence [ fmap (Right s,) <$> getTrace  s | s <- scripts ]
    misses   <- sequence [ (s,) <$> traceMisses t hashFile | Just (s, t) <- traces ]
    graph    <- dependencyGraph dir
    let outOfDate     = [ x | (x, OutOfDate) <- statuses ]
        transitive    = dfs outOfDate graph
        mismatches    = Set.fromList [ (Left f, s) | (s, fs) <- misses, (f, _) <- fs ]
        statusMap     = Map.fromList (statuses ++ map (, OutOfDate) transitive)
        isUpToDate  x = Map.lookup x statusMap == Just UpToDate
        isOutOfDate x = Map.lookup x statusMap == Just OutOfDate
        isError     x = Map.lookup x statusMap == Just Error
        mismatch x y  = Set.member (min x y, max x y) mismatches
        style = (defaultStyleViaShow :: Style (Either FilePath FilePath) String)
            { graphName  = dir
            , preamble   = [ "node [fontname = consolas, shape = box]" ]
            , vertexName = \case
                Left file    -> toStandard file
                Right script -> takeBaseName script
            , vertexAttributes = \x -> [ "style"       := "rounded" | isLeft      x ]
                                    ++ [ "style"       := "filled"  | isRight     x ]
                                    ++ [ "fillcolor"   := "#d1ffd8" | isUpToDate  x ]
                                    ++ [ "fillcolor"   := "#fcd2ae" | isOutOfDate x ]
                                    ++ [ "fillcolor"   := "#e0c3c5" | isError     x ]
                                    ++ [ "peripheries" := "2"       | isError     x ]
            , edgeAttributes = \x y -> [ "style"       := "dashed"  | mismatch x y  ] }
    putStrLn $ export style graph

reset :: FilePath -> IO ()
reset dir = removeFiles dir ["//*.stroll", "//*.stderr", "//*.stdout"]
