module Mammut.Commands.Backup
  ( backupCommand
  ) where

import Control.Eff
import Control.Eff.Exception (Exc, throwError)
import Control.Eff.Lift (Lift, lift)
import Control.Lens
import Control.Monad (forM)

import Data.Time (UTCTime)

import System.FilePath ((</>))

import Mammut.Crypto
import Mammut.Errors
import Mammut.FileSystem hiding (Directory)
import Mammut.Vault

backupCommand :: ( Member (Exc MammutError) r, Member FileSystem r
                 , Member VaultOp r )
              => Key -> FilePath -> FilePath -> UTCTime -> Eff r Version
backupCommand key location dir time = do
    let vault = Vault key location
    hash <- snd <$> go True vault dir
    writeVersion vault time hash

  where
    go :: (Member (Exc MammutError) r, Member FileSystem r, Member VaultOp r)
       => Bool -> Vault -> FilePath -> Eff r (ObjectType, ObjectHash)
    go firstCall vault path = do
      eRes <- readFileOrDirectory path
      case eRes of
        Left contents
          | firstCall -> throwError CantBackupFile
          | otherwise -> (PlainObject,) <$> writePlainObject vault contents
        Right names -> do
          items <- forM names $ \name -> do
            let path' = path </> name
            (typ, hash') <- go False vault path'
            return $ DirectoryItem name typ hash'
          (DirectoryObject,) <$> writeDirectory vault (Directory items)
