module Mammut.Commands.ListVersions
  ( listVersionsCommand
  ) where

import Control.Eff
import Control.Eff.Exception (Exc)
import Control.Eff.Lift (Lift, lift)
import Control.Lens
import Control.Monad (forM_)

import Data.Function (on)
import Data.List (sortBy)
import Data.Time (defaultTimeLocale, formatTime)

import Mammut.Crypto
import Mammut.Errors
import Mammut.Vault

listVersionsCommand :: ( Member (Exc MammutError) r, Member VaultOp r
                       , SetMember Lift (Lift IO) r )
                    => Key -> FilePath -> Eff r ()
listVersionsCommand key location = do
  versions <- readVersions $ Vault key location
  let sortedVersions = sortBy (compare `on` view versionTime) versions
  lift $ forM_ sortedVersions $ \version -> do
    putStrLn $ version ^. versionName ++ "\t"
               ++ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S"
                             (version ^. versionTime)
               ++ "\t" ++ version ^. versionHash
