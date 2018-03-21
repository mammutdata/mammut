module Mammut.Crypto
  ( Signed
  , fromSigned
  , Key
  , generateKey
  , parseSigned
  , writeSigned
  , hashFile
  , encryptFile
  , decryptFile
  ) where

import           Control.Monad (unless)

import           Data.Attoparsec.ByteString (Parser)
import           Data.ByteArray (convert)
import           Data.ByteArray.Encoding (convertToBase, Base(Base16))
import           Data.Monoid
import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

import           Crypto.Cipher.AES (AES256)
import           Crypto.Cipher.Types (BlockCipher(..))
import           Crypto.Hash (hashDigestSize, hashlazy)
import           Crypto.Hash.Algorithms (SHA256, SHA3_512(..))
import           Crypto.MAC.HMAC (HMAC, hmac)
import           Crypto.Random.Types (getRandomBytes)

import           Mammut.Crypto.Internal
import           Mammut.Errors
import           Mammut.Vault

-- | Generate a 256-bits long key.
generateKey :: IO Key
generateKey = Key <$> getRandomBytes 32

-- | Parse some contents preceded by a signature.
parseSigned :: Key -> Parser a -> Parser (Signed a)
parseSigned key parser = do
  signature <- A.take $ hashDigestSize SHA3_512 * 2 -- encoded in hexadecimal
  _         <- A.endOfLine
  contents  <- A.takeByteString

  let signature' = sign key contents

  unless (signature == signature') $ fail "invalid signature"
  either fail (return . Signed) $ A.parseOnly parser contents

-- | Write a signed value with its signature so that it can be read by
-- 'parseSigned'.
writeSigned :: Key -> (a -> BS.ByteString) -> Signed a -> BS.ByteString
writeSigned key write (Signed value) =
  let contents  = write value
      signature = sign key contents
  in signature <> "\n" <> contents

-- | Sign some contents with HMAC-SHA3-512 and encode the signature in
-- hexidecimal.
sign :: Key -> BS.ByteString -> BS.ByteString
sign (Key key) contents =
  convertToBase @BS.ByteString Base16 $ convert @(HMAC SHA3_512) $
    hmac key contents

-- | Hash some contents with SHA256. Intended to be used in order to name
-- objects, hence it returns a 'String' that can become part of a filepath.
hashFile :: BSL.ByteString -> ObjectHash
hashFile = show . hashlazy @SHA256

-- | Encrypt file contents.
encryptFile :: Key -> BSL.ByteString -> IO (Either MammutError BSL.ByteString)
encryptFile key contents = do
  bytes <- getRandomBytes $ blockSize (undefined :: AES256)
  return $ (BSL.fromStrict bytes <>) <$> cfb8Encrypt key bytes contents

-- | Decrypt file contents.
decryptFile :: Key -> BSL.ByteString -> Either MammutError BSL.ByteString
decryptFile key full = do
  let blockSize64 = fromInteger . toInteger $ blockSize (undefined :: AES256)
      (bytes, encrypted) = BSL.splitAt blockSize64 full
  cfb8Decrypt key (BSL.toStrict bytes) encrypted
