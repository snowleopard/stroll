module Development.Stroll.Hash (Hash, hash, hashFile, toText) where

import qualified Crypto.Hash as Crypto
import Data.ByteArray.Encoding
import Data.ByteString (ByteString)
import Data.Text
import Data.Text.Encoding
import Data.Yaml
import System.Directory

import qualified Data.ByteString as ByteString

-- | A SHA-256 hash used for efficient comparison and indexing of file contents.
newtype Hash = Hash { getHash :: Crypto.Digest Crypto.SHA256 }
    deriving Eq

instance Show Hash where
    show = show . toByteString

instance ToJSON Hash where
    toJSON = String . toText

instance FromJSON Hash where
    parseJSON v = do
        text <- parseJSON v
        case convertFromBase Base16 (encodeUtf8 text) of
            Left err -> fail ("Cannot decode hash: " ++ err)
            Right bs -> case Crypto.digestFromByteString (bs :: ByteString) of
                Nothing  -> fail ("Cannot parse hash " ++ unpack text)
                Just res -> return (Hash res)

-- | Hash a 'ByteString'.
hash :: ByteString -> Hash
hash = Hash . Crypto.hash

-- TODO: Optimise via streaming IO.
-- | Hash the contents of a given file. Returns 'Nothing' if the file does not
-- exist.
hashFile :: FilePath -> IO (Maybe Hash)
hashFile file = do
    exists <- doesFileExist file
    if exists then Just . hash <$> ByteString.readFile file
              else return Nothing

-- | Pretty-print a 'Hash' as a base-16 encoded 'ByteString'.
toByteString :: Hash -> ByteString
toByteString = convertToBase Base16 . getHash

-- | Pretty-print a 'Hash' as a base-16 encoded 'Text'.
toText :: Hash -> Text
toText = decodeUtf8 . toByteString
