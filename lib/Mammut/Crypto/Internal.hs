module Mammut.Crypto.Internal where

import           GHC.IO (unsafeDupablePerformIO)

import           Foreign.ForeignPtr (withForeignPtr)
import           Foreign.Ptr
import           Foreign.Storable (Storable(..))

import           Data.Bits (xor)
import           Data.ByteArray (ScrubbedBytes)
import           Data.Int
import           Data.Word
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Lazy as BSL

import           Crypto.Error (CryptoFailable(..))
import           Crypto.Cipher.AES (AES256)
import           Crypto.Cipher.Types (BlockCipher(..), Cipher(..))

import           Mammut.Errors

newtype Key = Key ScrubbedBytes deriving (Eq, Show)

-- | Encrypt some lazy bytestring.
cfbEncryptLazy :: Key -> BS.ByteString -> BSL.ByteString
               -> Either MammutError BSL.ByteString
cfbEncryptLazy (Key key) bytes contents =
    case cipherInit key of
      CryptoFailed err -> Left $ CryptoniteError err
      CryptoPassed cipher ->
        Right $ BSL.fromChunks $ go cipher 0 bytes contents

  where
    go :: AES256 -> Int -> BS.ByteString -> BSL.ByteString -> [BS.ByteString]
    go _ size _ "" = [BS.singleton . fromInteger . toInteger $ size]
    go cipher _ iv plain =
      let (blockLazy, plain') = BSL.splitAt (blockSize64 cipher) plain
          block               = BSL.toStrict blockLazy
          encIV               = ecbEncrypt cipher iv
          encBlock            = zipWithBS xor encIV block
      in encBlock : go cipher (BS.length block) encBlock plain'

-- | Decrypt a lazy bytestring if possible.
--
-- This function might return wrong data at the end, so the decrypted contents
-- should be checked with the hash.
cfbDecryptLazy :: Key -> BS.ByteString -> BSL.ByteString
               -> Either MammutError BSL.ByteString
cfbDecryptLazy (Key key) bytes encrypted = do
    case cipherInit key of
      CryptoFailed err -> Left $ CryptoniteError err
      CryptoPassed cipher ->
        Right $ BSL.fromChunks $ go cipher "" bytes encrypted

  where
    go :: AES256 -> BS.ByteString -> BS.ByteString -> BSL.ByteString
       -> [BS.ByteString]
    go _ _ _ "" = [] -- Should never happen
    go cipher previousBlock iv encrypted =
      case BSL.splitAt (blockSize64 cipher) encrypted of
        (sizeBS, "") ->
          let size = case BSL.uncons sizeBS of
                Just (size8, _) -> fromInteger $ toInteger size8
                Nothing         -> 0 -- Fallback value as we can't fail
          in [BS.take size previousBlock]
        (encBlockLazy, plain') ->
          let encBlock = BSL.toStrict encBlockLazy
              encIV    = ecbEncrypt cipher iv
              block    = zipWithBS xor encIV encBlock
          in previousBlock : go cipher block encBlock plain'

-- | Zip two bytestrings byte-wise and pad with '\0's if the first one is longer
-- than the other.
zipWithBS :: (Word8 -> Word8 -> Word8) -> BS.ByteString -> BS.ByteString
          -> BS.ByteString
zipWithBS f (BSI.PS ptr offset len) (BSI.PS ptr' offset' len') =
    unsafeDupablePerformIO $
      withForeignPtr ptr $ \a -> withForeignPtr ptr' $ \a' ->
        BSI.create len $ go 0 (a `plusPtr` offset) (a' `plusPtr` offset')

  where
    go :: Int -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO ()
    go n src src' dst
      | n >= len = return ()
      | otherwise = do
          x <- peekByteOff src n
          y <- if n < len' then peekByteOff src' n else return 0
          pokeByteOff dst n (f x y)
          go (n+1) src src' dst

blockSize64 :: BlockCipher a => a -> Int64
blockSize64 = fromInteger . toInteger . blockSize
