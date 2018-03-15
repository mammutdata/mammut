module Mammut.CryptoTest
  ( cryptoTests
  ) where

import qualified Data.Attoparsec.ByteString as A

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           Mammut.Crypto
import           Mammut.Crypto.Internal

import           TestHelpers

cryptoTests :: TestTree
cryptoTests = testGroup "Mammut.Crypto"
  [ testProperty "writeSigned and parseSigned are reciprocal" $ property $ do
      contents <- forAll $ Gen.bytes $ Range.linear 0 1000
      key      <- forAll encryptionKeyGen

      let signed = writeSigned key id (Signed contents)
      A.parseOnly (parseSigned key A.takeByteString) signed
        === Right (Signed contents)
  ]
