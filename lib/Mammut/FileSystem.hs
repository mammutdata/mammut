module Mammut.FileSystem
  ( FileType(..)
  , FileSystem
  , checkFile
  , readFile
  , listDirectory
  , writeFile
  , readFileOrDirectory
  , runFileSystem
  ) where

import           Prelude hiding (readFile, writeFile)

import           Control.Eff
import           Control.Eff.Exception
import           Control.Eff.Lift
import           Control.Monad.Trans (MonadIO, liftIO)

import qualified Data.ByteString.Lazy as BSL

import           Mammut.Errors
import           Mammut.FileSystem.Internal

checkFile :: Member FileSystem r => FilePath -> Eff r (Maybe FileType)
checkFile = send . CheckFile

readFile :: (Member (Exc MammutError) r, Member FileSystem r)
         => FilePath -> Eff r BSL.ByteString
readFile path = do
  eContents <- send $ ReadFile path
  liftEither eContents

listDirectory :: (Member (Exc MammutError) r, Member FileSystem r)
              => FilePath -> Eff r [FilePath]
listDirectory path = do
  eNames <- send $ ListDirectory path
  liftEither eNames

writeFile :: (Member (Exc MammutError) r, Member FileSystem r)
          => FilePath -> BSL.ByteString -> Eff r ()
writeFile path contents = do
  eRes <- send $ WriteFile path contents
  liftEither eRes

readFileOrDirectory :: (Member (Exc MammutError) r, Member FileSystem r)
                    => FilePath -> Eff r (Either BSL.ByteString [FilePath])
readFileOrDirectory path = do
  mType <- checkFile path
  case mType of
    Nothing        -> throwError $ FileNotFound path
    Just PlainFile -> Left  <$> readFile path
    Just Directory -> Right <$> listDirectory path

runFileSystem :: (MonadIO m, SetMember Lift (Lift m) r)
              => Eff (FileSystem ': r) a -> Eff r a
runFileSystem = handle_relay return $ \action rest -> case action of
  CheckFile path -> do
    mType <- lift $ liftIO $ checkFileIO path
    rest mType

  ReadFile path -> do
    eContents <- lift $ liftIO $ readFileIO path
    rest eContents

  ListDirectory path -> do
    eNames <- lift $ liftIO $ listDirectoryIO path
    rest eNames

  WriteFile path contents -> do
    eRes <- lift $ liftIO $ writeFileIO path contents
    rest eRes
