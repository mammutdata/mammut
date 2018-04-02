module Mammut.Vault.Operations
  ( VaultOp
  , runVaultOp
  , readVersions
  , readPlainObject
  , readDirectory
  , writeVersion
  , writePlainObject
  , writeDirectory
  ) where

import           Control.Eff
import           Control.Eff.Exception (Exc)
import           Control.Eff.Lift (Lift, lift)
import           Control.Monad.Trans (MonadIO, liftIO)

import           Data.Time (UTCTime)
import qualified Data.ByteString.Lazy as BSL

import           Mammut.Crypto
import           Mammut.Errors
import           Mammut.FileSystem
import           Mammut.Vault.Operations.Internal
import           Mammut.Vault.Types

readVersions :: Member VaultOp r => Vault -> Eff r [Version]
readVersions = send . ReadVersions

readPlainObject :: Member VaultOp r => Vault -> ObjectHash
                -> Eff r BSL.ByteString
readPlainObject vault hash = send $ ReadPlainObject vault hash

readDirectory :: Member VaultOp r => Vault -> ObjectHash -> Eff r Directory
readDirectory vault hash = send $ ReadDirectory vault hash

writeVersion :: Member VaultOp r => Vault -> UTCTime -> ObjectHash
             -> Eff r Version
writeVersion vault time hash = send $ WriteVersion vault time hash

writePlainObject :: Member VaultOp r => Vault -> BSL.ByteString
                 -> Eff r ObjectHash
writePlainObject vault contents = send $ WritePlainObject vault contents

writeDirectory :: Member VaultOp r => Vault -> Directory -> Eff r ObjectHash
writeDirectory vault dir = send $ WriteDirectory vault dir

runVaultOp :: ( Member (Exc MammutError) r, Member FileSystem r, MonadIO m
              , SetMember Lift (Lift m) r )
           => Eff (VaultOp ': r) a -> Eff r a
runVaultOp = handle_relay return $ \action rest -> case action of
  ReadVersions vault -> readVersions_ vault >>= rest

  ReadPlainObject vault hash -> readPlainObject_ vault hash >>= rest

  ReadDirectory vault hash -> readDirectory_ vault hash >>= rest

  WriteVersion vault time hash -> writeVersion_ vault time hash >>= rest

  WritePlainObject vault contents -> do
    iv <- lift $ liftIO generateIV
    hash <- writePlainObject_ vault iv contents
    rest hash

  WriteDirectory vault dir -> do
    iv <- lift $ liftIO generateIV
    hash <- writeDirectory_ vault iv dir
    rest hash
