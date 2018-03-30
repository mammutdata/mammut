{-# LANGUAGE UndecidableInstances #-}

module TestHelpers
  ( module Test.Tasty
  , module Test.Tasty.HUnit
  , module Test.Tasty.Hedgehog
  , module Hedgehog
  , module TestHelpers
  ) where

import           Control.Eff
import           Control.Eff.Exception (Exc, runError, throwError)
import           Control.Eff.Lift (Lift, lift, runLift)
import           Control.Eff.State.Strict (State, evalState, get, put)
import           Control.Lens
import           Control.Monad
import           Control.Monad.Trans hiding (lift)

import           Data.ByteArray (convert)
import           Data.Function (on)
import           Data.List
import           Data.Time
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashMap.Strict as HM

import           Crypto.Cipher.AES (AES256)
import           Crypto.Cipher.Types (BlockCipher(..))

import           System.FilePath
import           System.IO.Temp

import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.Hedgehog

import           Hedgehog hiding (assert)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           Mammut.Crypto.Internal
import           Mammut.Errors
import           Mammut.FileSystem.Internal hiding (FileType(..))
import           Mammut.Operations
import           Mammut.Operations.Internal
import           Mammut.Vault
import qualified Mammut.FileSystem as FS

{-
 - Generators
 -}

encryptionKeyGen :: Gen Key
encryptionKeyGen = fmap (Key . convert) $ Gen.bytes $ Range.singleton 32

encryptionIVGen :: Gen BS.ByteString
encryptionIVGen = Gen.bytes $ Range.singleton $ blockSize (undefined :: AES256)

utctimeGen :: Gen UTCTime
utctimeGen = do
  day  <- ModifiedJulianDay <$> Gen.integral (Range.linear 100000 999999)
  time <- secondsToDiffTime <$> Gen.integral (Range.linear 0 86400)
  return $ UTCTime day time

hashGen :: Gen ObjectHash
hashGen = Gen.string (Range.singleton 64) Gen.hexit

contentsGen :: Gen BSL.ByteString
contentsGen = fmap BSL.fromStrict . Gen.bytes $ Range.linear 0 1000

versionGen :: Gen Version
versionGen = Version <$> utctimeGen <*> hashGen

emptyVaultGen :: Gen Vault
emptyVaultGen = do
  key  <- encryptionKeyGen
  path <- Gen.string (Range.singleton 20) Gen.alphaNum
  return $ Vault key ("/path/to/vault" </> path) []

vaultGen :: Gen Vault
vaultGen = do
  vault <- emptyVaultGen
  allVersions <- Gen.list (Range.linear 0 10) versionGen
  let versions = sortOn (view versionTime) $
        nubBy ((==) `on` view versionTime) allVersions
  return $ vault & vaultVersions .~ versions

directoryItemGen :: Gen DirectoryItem
directoryItemGen = do
  name <- Gen.string (Range.linear 1 30) Gen.alphaNum
  typ  <- Gen.element [PlainObject, DirectoryObject]
  hash <- hashGen
  return $ DirectoryItem name typ hash

directoryGen :: Gen Directory
directoryGen = do
  items <- Gen.list (Range.linear 0 10) directoryItemGen
  let items' = nubBy ((==) `on` view itemName) items
  return $ Directory items'

{-
 - Test environment
 -}

instance (MonadTest m, SetMember Lift (Lift m) r) => MonadTest (Eff r) where
  liftTest = lift . liftTest

type FakeFS = HM.HashMap FilePath BSL.ByteString

inTestEnv :: MonadIO m => Eff '[Mammut, FileSystem, Exc MammutError, Lift m] a
          -> m (Either MammutError a)
inTestEnv =
    runLift . runError . flip evalState HM.empty
            . handle_relay return fakeFS . addState . runMammut
  where
    fakeFS :: MonadIO m => FileSystem a
           -> (a -> Eff '[State FakeFS, Exc MammutError, Lift m] b)
           -> Eff '[State FakeFS, Exc MammutError, Lift m] b
    fakeFS action rest = case action of
      CheckFile path -> do
        fs <- get
        if HM.member path (fs :: FakeFS)
          then rest $ Just FS.PlainFile
          else if any ((path ++ "/") `isPrefixOf`) (HM.keys fs)
                 then rest $ Just FS.Directory
                 else rest Nothing

      ReadFile path -> do
        fs <- get
        rest $ case HM.lookup path fs of
          Just contents -> Right contents
          Nothing -> Left $ FileNotFound path

      ListDirectory path -> do
        fs <- get
        let prefix = path ++ "/"
            names =
              nub . map (takeWhile (/= '/') . drop (length prefix))
              . filter (prefix `isPrefixOf`)
              $ HM.keys (fs :: FakeFS)
        rest $ if null names
          then Left $ FileNotFound path
          else Right names

      WriteFile path contents -> do
        fs <- get
        if any ((path ++ "/") `isPrefixOf`) (HM.keys fs)
          then rest $ Left $ CantWriteFile path "is a directory"
          else do
            put $ HM.insert path contents fs
            rest $ Right ()

    addState :: Eff (FileSystem ': r) a
             -> Eff (FileSystem ': State FakeFS ': r) a
    addState = \case
      Val x -> Val x
      E eff arr -> do
        let arr' = qComps arr addState
        case decomp eff of
          Left eff' -> E (weaken (weaken eff')) arr'
          Right fs -> E (inj fs) arr'

inTestEnv_ :: (MonadIO m, MonadTest m)
           => Eff '[Mammut, FileSystem, Exc MammutError, Lift m] a -> m ()
inTestEnv_ action = do
  eRes <- inTestEnv action
  _ <- evalEither eRes
  return ()
