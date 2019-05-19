module Development.Stroll.Base where

import Data.ByteString (ByteString)

-- Vocabulary of build systems
type Key    = FilePath
type Value  = ByteString
type Script = FilePath
