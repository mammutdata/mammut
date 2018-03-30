module Mammut.OperationsTest
  ( operationsTests
  ) where

import Control.Lens
import Control.Monad.Trans

import Mammut.Crypto.Internal
import Mammut.Operations
import Mammut.Vault

import TestHelpers

operationsTests :: TestTree
operationsTests = testGroup "Mammut.Operations"
  [ testProperty "writeVault and readVault are inverses" $ property $ do
      vault <- forAll vaultGen

      inTestEnv_ $ do
        writeVault vault
        vault' <- readVault (vault ^. vaultKey) (vault ^. vaultLocation)
        vault === vault'

  , testProperty "writePlainObject and readPlainObject are\
                 \ inverses" $ property $ do
      vault    <- forAll emptyVaultGen
      contents <- forAll contentsGen

      inTestEnv_ $ do
        hash <- writePlainObject vault contents
        contents' <- readPlainObject vault hash
        contents' === contents

  , testProperty "writeDirectory and readDirectory are inverses" $ property $ do
      vault     <- forAll emptyVaultGen
      directory <- forAll directoryGen

      inTestEnv_ $ do
        hash <- writeDirectory vault directory
        directory' <- readDirectory vault hash
        directory' === Signed directory -- FIXME: order
  ]
