module Mammut.Errors
  ( MammutError(..)
  ) where

import Crypto.Error (CryptoError)

data MammutError
  = CryptoniteError CryptoError
  -- ^ Error coming from the underlying crypto library.
  | CorruptedFile FilePath
  -- ^ The file's contents doesn't match its hash.
  | UnreadableDirectory String
  -- ^ Error while parsing a directory.
  | FileNotFound FilePath
  -- ^ Invalid file path.
  | CantWriteFile FilePath String
  -- ^ The program tried to write contents to a file but failed.
  deriving (Eq, Show)
