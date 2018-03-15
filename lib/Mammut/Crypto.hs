module Mammut.Crypto
  ( Signed
  , fromSigned
  , Key
  , generateKey
  , parseSigned
  , writeSigned
  , hashFile
  ) where

import           Control.Monad (unless)

import           Data.Attoparsec.ByteString (Parser)
import           Data.ByteArray (convert)
import           Data.ByteArray.Encoding (convertToBase, Base(Base16))
import           Data.Monoid
import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

import           Crypto.Hash (hashlazy)
import           Crypto.Hash.Algorithms (SHA3_512, SHA256)
import           Crypto.MAC.HMAC (HMAC, hmac)
import           Crypto.Random.Types (getRandomBytes)

import           Mammut.Crypto.Internal

-- | Generate a 64-bytes long key.
generateKey :: IO Key
generateKey = Key <$> getRandomBytes 64

-- | Parse some contents preceded by a signature.
parseSigned :: Key -> Parser a -> Parser (Signed a)
parseSigned key parser = do
  signature <- A.take 128
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
hashFile :: BSL.ByteString -> String
hashFile = show . hashlazy @SHA256
