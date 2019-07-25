import Development.Stroll
import System.Directory
import System.Environment

-- Compiling hello world:
-- reads: 1031
-- writes: 27
-- deletes: 18
-- Set size : 431

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
