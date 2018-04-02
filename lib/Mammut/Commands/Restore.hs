module Mammut.Commands.Restore
  ( restoreCommand
  ) where

import Prelude hiding (writeFile)

import Control.Eff
import Control.Eff.Exception (Exc, throwError)
import Control.Lens
import Control.Monad (forM_)

import Data.List (find)

import System.FilePath ((</>))

import Mammut.Crypto
import Mammut.Errors
import Mammut.FileSystem
import Mammut.Vault

restoreCommand :: ( Member (Exc MammutError) r, Member FileSystem r
                  , Member VaultOp r )
               => Key -> FilePath -> String -> FilePath -> Eff r ()
restoreCommand key location name dir = do
    let vault = Vault key location
    versions <- readVersions vault

    version <- case find ((==name) . view versionName) versions of
      Just v -> return v
      Nothing -> throwError $ FileNotFound name

    restoreDirectory vault (version ^. versionHash) dir

  where
    restoreDirectory :: ( Member (Exc MammutError) r, Member FileSystem r
                        , Member VaultOp r )
                     => Vault -> ObjectHash -> FilePath -> Eff r ()
    restoreDirectory vault hash dest = do
      directory <- readDirectory vault hash
      forM_ (directory ^. directoryItems) $ \item -> do
        let dest' = dest </> item ^. itemName
            restore = case item ^. itemType of
              PlainObject     -> restoreFile
              DirectoryObject -> restoreDirectory
        restore vault (item ^. itemHash) dest'

    restoreFile :: ( Member (Exc MammutError) r, Member FileSystem r
                   , Member VaultOp r )
                => Vault -> ObjectHash -> FilePath -> Eff r ()
    restoreFile vault hash dest = do
      contents <- readPlainObject vault hash
      writeFile dest contents
