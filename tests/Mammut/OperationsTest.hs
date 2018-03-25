module Mammut.OperationsTest
  ( operationsTests
  ) where

import Control.Lens
import Control.Monad.Trans

import Mammut.Crypto.Internal
import Mammut.Operations.Internal
import Mammut.Vault

import TestHelpers

operationsTests :: TestTree
operationsTests = testGroup "Mammut.Operations"
  [ ioTests ]

ioTests :: TestTree
ioTests = testGroup "IO operations"
  [ testProperty "writeVaultIO and readVaultIO are inverses" $ property $ do
      dir   <- createTestDirectory
      vault <- forAll $ vaultGen dir

      -- Other tests might have used the same directory
      liftIO $ removeDirectoryRecursive' $ vault ^. vaultLocation

      liftIO $ writeVaultIO vault
      vault' <- liftIO $
        readVaultIO (vault ^. vaultKey) (vault ^. vaultLocation)
      vault === vault'

  , testProperty "writePlainObjectIO and readPlainObjectIO are\
                 \ inverses" $ property $ do
      dir      <- createTestDirectory
      vault    <- forAll $ emptyVaultGen dir
      contents <- forAll contentsGen

      -- Other tests might have used the same directory
      liftIO $ removeDirectoryRecursive' $ vault ^. vaultLocation

      eHash <- liftIO $ writePlainObjectIO vault contents
      hash  <- evalEither eHash
      eContents <- liftIO $ readPlainObjectIO vault hash
      eContents === Right contents

  , testProperty "writeDirectoryIO and readDirectoryIO are\
                 \ inverses" $ property $ do
      dir       <- createTestDirectory
      vault     <- forAll $ emptyVaultGen dir
      directory <- forAll directoryGen

      -- Other tests might have used the same directory
      liftIO $ removeDirectoryRecursive' $ vault ^. vaultLocation

      eHash <- liftIO $ writeDirectoryIO vault directory
      hash  <- evalEither eHash
      eDirectory <- liftIO $ readDirectoryIO vault hash
      eDirectory === Right (Signed directory) -- FIXME: order
  ]
