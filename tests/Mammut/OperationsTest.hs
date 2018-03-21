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
      case eHash of
        Left err -> do
          annotateShow err
          failure
        Right hash -> do
          eContents <- liftIO $ readPlainObjectIO vault hash
          eContents === Right contents
  ]
