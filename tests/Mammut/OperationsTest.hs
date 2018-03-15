module Mammut.OperationsTest
  ( operationsTests
  ) where

import Control.Lens
import Control.Monad.Trans

import Mammut.Operations.Internal
import Mammut.Vault

import TestHelpers

operationsTests :: TestTree
operationsTests = testGroup "Mammut.Operations"
  [ ioTests ]

ioTests :: TestTree
ioTests = testGroup "IO operations"
  [ testProperty "writeVault and readVault are reciprocal" $ property $ do
      dir   <- createTestDirectory
      key   <- forAll encryptionKeyGen
      vault <- forAll $ vaultGen dir

      -- Other tests might have used the same directory
      liftIO $ removeDirectoryRecursive' $ vault ^. vaultLocation

      liftIO $ writeVaultIO key vault
      vault' <- liftIO $ readVaultIO key $ vault ^. vaultLocation
      vault === vault'
  ]
