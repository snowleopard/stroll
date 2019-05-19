{-# LANGUAGE OverloadedStrings, RecordWildCards, ScopedTypeVariables #-}

module Development.Stroll where

import Development.Stroll.Base

import Development.Shake.Command
import System.FilePath

import qualified Data.Set as Set

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
