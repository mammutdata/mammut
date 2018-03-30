module Mammut.CryptoTest
  ( cryptoTests
  ) where

import           Control.Monad.Trans

import qualified Data.Attoparsec.ByteString.Lazy as A

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           Mammut.Crypto
import           Mammut.Crypto.Internal

import           TestHelpers

cryptoTests :: TestTree
cryptoTests = testGroup "Mammut.Crypto"
  [ testProperty "writeSigned and parseSigned are inverses" $ property $ do
      contents <- forAll contentsGen
      key      <- forAll encryptionKeyGen

      let signed = writeSigned key contents
      A.eitherResult (A.parse (parseSigned key A.takeLazyByteString) signed)
        === Right (Signed contents)

  , testProperty "encryptFile and decryptFile are inverses" $ property $ do
      contents <- forAll contentsGen
      key      <- forAll encryptionKeyGen
      iv       <- forAll encryptionIVGen

      let eRes = encryptFile key iv contents >>= decryptFile key
      eRes === Right contents
  ]
