module Mammut.Commands.GenerateKey
  ( generateKeyCommand
  ) where

import           Prelude hiding (writeFile)

import           Control.Eff
import           Control.Eff.Exception (Exc)
import           Control.Eff.Lift (Lift, lift)
import           Control.Monad
import           Control.Monad.Trans (MonadIO, liftIO)

import           Data.ByteArray (convert)
import           Data.Maybe
import qualified Data.ByteString.Lazy as BSL

import           Mammut.Crypto
import           Mammut.Crypto.Internal
import           Mammut.Errors
import           Mammut.FileSystem

-- FIXME: Set permissions to 600
generateKeyCommand :: ( Member (Exc MammutError) r, Member FileSystem r
                      , SetMember Lift (Lift m) r, MonadIO m )
                   => FilePath -> Eff r ()
generateKeyCommand path = do
  mType <- checkFile path
  when (isNothing mType) $ do
    Key key <- lift $ liftIO generateKey
    writeFile path $ BSL.fromStrict $ convert key
