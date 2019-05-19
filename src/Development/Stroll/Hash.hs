module Development.Stroll.Hash (Hash, hash, toText) where

import qualified Crypto.Hash as Crypto
import Data.ByteArray.Encoding
import Data.Text
import Data.Text.Encoding
import Data.ByteString (ByteString)

newtype Hash = Hash { getHash :: Crypto.Digest Crypto.SHA256 }
    deriving Eq

instance Show Hash where
    show = show . toByteString

hash :: ByteString -> Hash
hash = Hash . Crypto.hash

toByteString :: Hash -> ByteString
toByteString = convertToBase Base16 . getHash

toText :: Hash -> Text
toText = decodeUtf8 . toByteString
