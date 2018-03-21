module Mammut.Crypto.Internal where

import           Data.Bits (xor)
import           Data.ByteArray (ScrubbedBytes)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

import           Crypto.Error (CryptoFailable(..))
import           Crypto.Cipher.AES (AES256)
import           Crypto.Cipher.Types (BlockCipher(..), Cipher(..))

import           Mammut.Errors

-- | Signed entity. This type is use when parsing/writing something to a file.
newtype Signed a = Signed { fromSigned :: a } deriving (Eq, Show)

newtype Key = Key ScrubbedBytes deriving (Eq, Show)

cfb8Encrypt :: Key -> BS.ByteString -> BSL.ByteString
            -> Either MammutError BSL.ByteString
cfb8Encrypt (Key key) bytes contents = do
    cipher <- case cipherInit key of
      CryptoFailed e -> Left $ CryptoniteError e
      CryptoPassed c -> Right c
    return $ go cipher bytes contents

  where
    go :: AES256 -> BS.ByteString -> BSL.ByteString -> BSL.ByteString
    go _ _ "" = ""
    go cipher iv plain =
      let encByte = BS.head (ecbEncrypt cipher iv) `xor` BSL.head plain
          iv'     = BS.tail iv `BS.snoc` encByte
          plain'  = BSL.tail plain
      in BSL.cons encByte $ go cipher iv' plain'

cfb8Decrypt :: Key -> BS.ByteString -> BSL.ByteString
            -> Either MammutError BSL.ByteString
cfb8Decrypt (Key key) bytes encrypted = do
    cipher <- case cipherInit key of
      CryptoFailed e -> Left $ CryptoniteError e
      CryptoPassed c -> Right c
    return $ go cipher bytes encrypted

  where
    go :: AES256 -> BS.ByteString -> BSL.ByteString -> BSL.ByteString
    go _ _ "" = ""
    go cipher iv encrypted =
      let encByte    = BSL.head encrypted
          plainByte  = BS.head (ecbEncrypt cipher iv) `xor` encByte
          iv'        = BS.tail iv `BS.snoc` encByte
          encrypted' = BSL.tail encrypted
      in BSL.cons plainByte $ go cipher iv' encrypted'
