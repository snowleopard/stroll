{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

import Development.Stroll

import Crypto.Hash
import qualified Data.Map as Map
import System.Exit
import Data.ByteString
import Data.Yaml

-- Compiling hello world:
-- reads: 1031
-- writes: 27
-- deletes: 18
-- Set size : 431

{- [Note: Operations and Info]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

When Stroll executes a script, the file-system operations performed by the
script are recorded.

The current model allows only four operations for the sake of simplicity.

-- * Reading multiple times is OK, we assume all reads resulted in the same value
-- * Writing multiple times is OK, we only remember the latest value
-- Reading and writing the same key is called "modification", tracking the
-- latest written

-}



main :: IO ()
main = do
    -- run "default" "link.bat"
    let h     = hash ("123" :: ByteString)
        trace = Trace h
                      ExitSuccess
                      (Map.fromList [("qwe", Delete), ("xru", Read h)])
                      ""
                      ""
    encodeFile "test.stroll" trace

