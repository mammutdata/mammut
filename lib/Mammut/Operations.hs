module Mammut.Operations
  ( MammutRead
  , readVault
  , readPlainObject
  , readDirectory
  , writeVault
  , writePlainObject
  , writeDirectory
  ) where

import           Control.Eff

import qualified Data.ByteString.Lazy as BSL

import           Mammut.Crypto
import           Mammut.Operations.Internal
import           Mammut.Vault

readVault :: Member MammutRead r => FilePath -> Eff r Vault
readVault = send . ReadVault

readPlainObject :: Member MammutRead r => ObjectHash -> Eff r BSL.ByteString
readPlainObject = send . ReadPlainObject

readDirectory :: Member MammutRead r => ObjectHash -> Eff r (Signed Directory)
readDirectory = send . ReadDirectory

writeVault :: Member MammutWrite r => FilePath -> Vault -> Eff r ()
writeVault path vault = send $ WriteVault path vault

writePlainObject :: Member MammutWrite r => ObjectHash -> BSL.ByteString
                 -> Eff r ()
writePlainObject hash contents = send $ WritePlainObject hash contents

writeDirectory :: Member MammutWrite r => ObjectHash -> Signed Directory
               -> Eff r ()
writeDirectory hash dir = send $ WriteDirectory hash dir
