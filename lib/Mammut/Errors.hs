module Mammut.Errors
  ( MammutError(..)
  , errorMessage
  ) where

import Crypto.Error (CryptoError)

data MammutError
  = CryptoniteError CryptoError
  -- ^ Error coming from the underlying crypto library.
  | CorruptedFile FilePath
  -- ^ The file's contents doesn't match its hash.
  | UnreadableVersion FilePath String
  -- ^ Error while parsing version.
  | UnreadableDirectory FilePath String
  -- ^ Error while parsing a directory.
  | FileNotFound FilePath
  -- ^ Invalid file path.
  | CantWriteFile FilePath String
  -- ^ The program tried to write contents to a file but failed.
  | CantBackupFile
  -- ^ A backup needs to be a directory.
  | InvalidKey FilePath
  -- ^ Invalid encryption key.
  | VersionNotFound String
  -- ^ Version was not found in the vault.
  deriving (Eq, Show)

errorMessage :: MammutError -> String
errorMessage = \case
  CryptoniteError err -> "Encryption error: " ++ show err
  CorruptedFile path -> "Corrupted file in the vault: " ++ path
  UnreadableVersion path err ->
    "Corrupted version in the vault (" ++ path ++ "): " ++ err
  UnreadableDirectory path err ->
    "Corrupted directory in the vault (" ++ path ++ "): " ++ err
  FileNotFound path -> "File not found: " ++ path
  CantWriteFile path err ->
    "Attempt to write contents to a directory (" ++ path ++ "): " ++ err
  CantBackupFile ->
    "Can't back up a single file. Please put it inside a directory."
  InvalidKey path -> "Invalid encryption key: " ++ path
  VersionNotFound name -> "Version not found: " ++ name
