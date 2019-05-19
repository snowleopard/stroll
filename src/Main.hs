{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

import Data.Functor
import Development.Stroll.Script
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
        (file:_) -> void (execute file)
        _ -> putStrLn "Please specify a directory to stroll"

