module TestHelpers
  ( module Test.Tasty
  , module Test.Tasty.HUnit
  , module Test.Tasty.Hedgehog
  , module Hedgehog
  , module TestHelpers
  ) where

import           Control.Lens
import           Control.Monad
import           Control.Monad.Trans

import           Data.ByteArray (convert)
import           Data.Function (on)
import           Data.List
import           Data.Time
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

import           System.Directory
import           System.FilePath
import           System.IO.Temp

import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.Hedgehog

import           Hedgehog hiding (assert)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           Mammut.Crypto.Internal
import           Mammut.Vault

encryptionKeyGen :: Gen Key
encryptionKeyGen = fmap (Key . convert) $ Gen.bytes $ Range.singleton 32

utctimeGen :: Gen UTCTime
utctimeGen = do
  day  <- ModifiedJulianDay <$> Gen.integral (Range.linear 100000 999999)
  time <- secondsToDiffTime <$> Gen.integral (Range.linear 0 86400)
  return $ UTCTime day time

hashGen :: Gen ObjectHash
hashGen = Gen.string (Range.singleton 64) Gen.hexit

contentsGen :: Gen BSL.ByteString
contentsGen = fmap BSL.fromStrict . Gen.bytes $ Range.linear 0 10000

versionGen :: Gen Version
versionGen = Version <$> utctimeGen <*> hashGen

emptyVaultGen :: FilePath -> Gen Vault
emptyVaultGen dir = do
  key  <- encryptionKeyGen
  path <- (dir </>) <$> Gen.string (Range.singleton 20) Gen.alphaNum
  return $ Vault key path []

vaultGen :: FilePath -> Gen Vault
vaultGen dir = do
  vault <- emptyVaultGen dir
  allVersions <- Gen.list (Range.linear 0 10) versionGen
  let versions = map Signed $ sortOn (view versionTime) $
        nubBy ((==) `on` view versionTime) allVersions
  return $ vault & vaultVersions .~ versions

createTestDirectory :: MonadIO m => m FilePath
createTestDirectory = liftIO $ do
  tmp <- getCanonicalTemporaryDirectory
  let base = tmp </> "mammut-tests"
  removeDirectoryRecursive base
  createDirectoryIfMissing False base
  createTempDirectory tmp $ base </> "dir"

removeDirectoryRecursive' :: MonadIO m => FilePath -> m ()
removeDirectoryRecursive' path = liftIO $ do
  exists <- doesDirectoryExist path
  when exists $ removeDirectoryRecursive path
