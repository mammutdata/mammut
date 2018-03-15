module TestHelpers
  ( module Test.Tasty
  , module Test.Tasty.HUnit
  , module Test.Tasty.Hedgehog
  , module Hedgehog
  , module TestHelpers
  ) where

import           Control.Lens (view)
import           Control.Monad
import           Control.Monad.Trans

import           Data.Function (on)
import           Data.List
import           Data.Time

import           System.Directory
import           System.FilePath
import           System.IO.Temp

import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.Hedgehog hiding (assert)

import           Hedgehog hiding (assert)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           Mammut.Crypto.Internal
import           Mammut.Vault

encryptionKeyGen :: Gen Key
encryptionKeyGen = fmap Key $ Gen.bytes $ Range.singleton 64

utctimeGen :: Gen UTCTime
utctimeGen = do
  day  <- ModifiedJulianDay <$> Gen.integral (Range.linear 100000 999999)
  time <- secondsToDiffTime <$> Gen.integral (Range.linear 0 86400)
  return $ UTCTime day time

hashGen :: Gen ObjectHash
hashGen = Gen.string (Range.singleton 64) Gen.hexit

versionGen :: Gen Version
versionGen = Version <$> utctimeGen <*> hashGen

vaultGen :: FilePath -> Gen Vault
vaultGen dir = do
  path     <- (dir </>) <$> Gen.string (Range.singleton 20) Gen.alphaNum
  versions <- nubBy ((==) `on` view (fromSigned . versionTime))
    <$> Gen.list (Range.linear 0 10) (Signed <$> versionGen)
  return $ Vault path $ sortOn (view (fromSigned . versionTime)) versions

createTestDirectory :: MonadIO m => m FilePath
createTestDirectory = liftIO $ do
  tmp <- getCanonicalTemporaryDirectory
  let base = tmp </> "mammut-tests"
  removeDirectoryRecursive base
  createDirectoryIfMissing False base
  createTempDirectory tmp $ base </> "dir"

removeDirectoryRecursive' :: MonadIO m => FilePath -> m ()
removeDirectoryRecursive' path = liftIO $ do
  check <- doesDirectoryExist path
  when check $ removeDirectoryRecursive path
