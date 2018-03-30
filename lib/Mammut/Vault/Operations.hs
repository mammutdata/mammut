module Mammut.Vault.Operations
  ( VaultOp
  , runVaultOp
  , readVault
  , readPlainObject
  , readDirectory
  , writeVault
  , writePlainObject
  , writeDirectory
  ) where

import           Control.Eff
import           Control.Eff.Exception (Exc, throwError)
import           Control.Eff.Lift (Lift, lift)
import           Control.Monad.Trans (MonadIO, liftIO)

import qualified Data.ByteString.Lazy as BSL

import           Mammut.Crypto
import           Mammut.Errors
import           Mammut.FileSystem
import           Mammut.Vault.Operations.Internal
import           Mammut.Vault.Types

readVault :: Member VaultOp r => Key -> FilePath -> Eff r Vault
readVault key path = send $ ReadVault key path

readPlainObject :: Member VaultOp r => Vault -> ObjectHash
                -> Eff r BSL.ByteString
readPlainObject vault hash = send $ ReadPlainObject vault hash

readDirectory :: Member VaultOp r => Vault -> ObjectHash
              -> Eff r (Signed Directory)
readDirectory vault hash = send $ ReadDirectory vault hash

writeVault :: Member VaultOp r => Vault -> Eff r ()
writeVault = send . WriteVault

writePlainObject :: Member VaultOp r => Vault -> BSL.ByteString
                 -> Eff r ObjectHash
writePlainObject vault contents = send $ WritePlainObject vault contents

writeDirectory :: Member VaultOp r => Vault -> Directory -> Eff r ObjectHash
writeDirectory vault dir = send $ WriteDirectory vault dir

runVaultOp :: ( Member (Exc MammutError) r, Member FileSystem r, MonadIO m
              , SetMember Lift (Lift m) r )
           => Eff (VaultOp ': r) a -> Eff r a
runVaultOp = handle_relay return $ \action rest -> case action of
  ReadVault key path -> readVault_ key path >>= rest

  ReadPlainObject vault hash -> do
    eContents <- readPlainObject_ vault hash
    case eContents of
      Left err -> throwError err
      Right contents -> rest contents

  ReadDirectory vault hash -> do
    eDirectory <- readDirectory_ vault hash
    case eDirectory of
      Left err -> throwError err
      Right dir -> rest dir

  WriteVault vault -> writeVault_ vault >>= rest

  WritePlainObject vault contents -> do
    iv <- lift $ liftIO generateIV
    eHash <- writePlainObject_ vault iv contents
    case eHash of
      Left err -> throwError err
      Right hash -> rest hash

  WriteDirectory vault dir -> do
    iv <- lift $ liftIO generateIV
    eHash <- writeDirectory_ vault iv dir
    case eHash of
      Left err -> throwError err
      Right hash -> rest hash
