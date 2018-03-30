module Mammut.FileSystem.Internal where

import           Control.Exception (catch, throw)

import           System.FilePath (takeDirectory)
import           System.Directory
import           System.IO
import           System.IO.Error (isDoesNotExistError)

import qualified Data.ByteString.Lazy as BSL

import           Mammut.Errors

data FileType = PlainFile | Directory

data FileSystem a where
  CheckFile     :: FilePath -> FileSystem (Maybe FileType)
  ReadFile      :: FilePath -> FileSystem (Either MammutError BSL.ByteString)
  ListDirectory :: FilePath -> FileSystem (Either MammutError [FilePath])
  WriteFile     :: FilePath -> BSL.ByteString
                -> FileSystem (Either MammutError ())

checkFileIO :: FilePath -> IO (Maybe FileType)
checkFileIO path = do
  isFile <- doesFileExist path
  if isFile
    then return $ Just PlainFile
    else do
      isDir <- doesDirectoryExist path
      if isDir
        then return $ Just Directory
        else return Nothing

readFileIO :: FilePath -> IO (Either MammutError BSL.ByteString)
readFileIO path = do
  (Right <$> BSL.readFile path)
    `catch` \e -> if isDoesNotExistError e
                    then return $ Left $ FileNotFound path
                    else throw e

listDirectoryIO :: FilePath -> IO (Either MammutError [FilePath])
listDirectoryIO path = do
  (Right <$> listDirectory path)
    `catch` \e -> if isDoesNotExistError e
                    then return $ Left $ FileNotFound path
                    else throw e

writeFileIO :: FilePath -> BSL.ByteString -> IO (Either MammutError ())
writeFileIO path contents = do
  createDirectoryIfMissing True $ takeDirectory path
  (Right <$> BSL.writeFile path contents)
    `catch` (return . Left . CantWriteFile path . show @IOError)
