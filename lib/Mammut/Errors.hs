module Mammut.Errors
  ( MammutError(..)
  ) where

import Crypto.Error (CryptoError)

data MammutError
  = CryptoniteError CryptoError
  -- ^ Error coming from the underlying crypto library.
  | CorruptedFile FilePath
  -- ^ The file's contents doesn't match its hash.
  deriving (Eq, Show)
