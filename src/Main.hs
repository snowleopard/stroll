import Development.Stroll
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
        (dir:_) -> stroll dir
        _ -> putStrLn "Please specify a directory to stroll"

