module Mammut.Crypto
  ( Key
  , generateKey
  , keyFromBS
  , keyFromFile
  , generateIV
  , nextIV
  , parseSigned
  , writeSigned
  , hashFile
  , encryptFile
  , decryptFile
  ) where

import           Prelude hiding (readFile)

import           Control.Eff
import           Control.Eff.Exception (Exc, throwError)
import           Control.Monad (unless)

import           Data.Attoparsec.ByteString.Lazy (Parser, parse, eitherResult)
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
import           Crypto.MAC.HMAC (HMAC, finalize, initialize, updates)
import           Crypto.Random.Types (getRandomBytes)

import           Mammut.Crypto.Internal
import           Mammut.Errors
import           Mammut.FileSystem
import           Mammut.Vault.Types

-- | Generate a 256-bits long key.
generateKey :: IO Key
generateKey = Key <$> getRandomBytes 32

-- | Return a key from the given bytestring.
keyFromBS :: BS.ByteString -> Maybe Key
keyFromBS bs | BS.length bs == 32 = Just $ Key $ convert bs
             | otherwise = Nothing

-- | Read a key from a file.
keyFromFile :: (Member (Exc MammutError) r, Member FileSystem r) => FilePath
            -> Eff r Key
keyFromFile path = do
  contents <- BSL.toStrict <$> readFile path
  case keyFromBS contents of
    Nothing  -> throwError $ InvalidKey path
    Just key -> return key

-- | Generate a initialisation vector for AES256 encryption.
generateIV :: IO BS.ByteString
generateIV = getRandomBytes $ blockSize (undefined :: AES256)

-- | Return a new initialisation vector from another and an encrypted text.
nextIV :: BS.ByteString -> BSL.ByteString -> BS.ByteString
nextIV iv enc =
  let size = blockSize64 (undefined :: AES256)
  in BSL.toStrict . BSL.take size $ BSL.drop size enc <> BSL.fromStrict iv

-- | Parse some contents preceded by a signature.
parseSigned :: Key -> Parser a -> Parser a
parseSigned key parser = do
  signature <- A.take $ hashDigestSize SHA3_512 * 2 -- encoded in hexadecimal
  _         <- A.endOfLine
  contents  <- A.takeLazyByteString

  let signature' = sign key contents

  unless (BSL.fromStrict signature == signature') $ fail "invalid signature"
  either fail return $ eitherResult $ parse parser contents

-- | Write a signed value with its signature so that it can be read by
-- 'parseSigned'.
writeSigned :: Key -> BSL.ByteString -> BSL.ByteString
writeSigned key contents = sign key contents <> "\n" <> contents

-- | Sign some contents with HMAC-SHA3-512 and encode the signature in
-- hexidecimal.
sign :: Key -> BSL.ByteString -> BSL.ByteString
sign (Key key) contents =
  BSL.fromStrict
  . convertToBase @BS.ByteString Base16
  . convert @(HMAC SHA3_512)
  . finalize
  . flip updates (BSL.toChunks contents)
  $ initialize key

-- | Hash some contents with SHA256. Intended to be used in order to name
-- objects, hence it returns a 'String' that can become part of a filepath.
hashFile :: BSL.ByteString -> ObjectHash
hashFile = show . hashlazy @SHA256

-- | Encrypt file contents with the given key and IV.
encryptFile :: Key -> BS.ByteString -> BSL.ByteString
            -> Either MammutError BSL.ByteString
encryptFile key iv contents = do
  encrypted <- cfbEncryptLazy key iv contents
  return $ BSL.fromStrict iv <> encrypted

-- | Decrypt file contents.
decryptFile :: Key -> BSL.ByteString -> Either MammutError BSL.ByteString
decryptFile key full = do
  let (iv, encrypted) = BSL.splitAt (blockSize64 (undefined :: AES256)) full
  cfbDecryptLazy key (BSL.toStrict iv) encrypted
