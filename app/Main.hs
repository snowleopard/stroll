import Development.Stroll
import System.Directory
import System.Environment

-- TODO: Switch to a proper command line flags parser.
main :: IO ()
main = do
    args <- getArgs
    case args of
        ("-i":dir   :_) -> info  dir
        ("-g":dir   :_) -> graph dir
        ("-r":dir   :_) -> reset dir
        (     target:_) -> do isDir <- doesDirectoryExist target
                              if isDir then stroll target else step target
        _ -> putStrLn "Please specify a directory to stroll"
