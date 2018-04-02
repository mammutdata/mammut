module Mammut.Vault.OperationsTest
  ( operationsTests
  ) where

import Control.Lens
import Control.Monad.Trans

import Mammut.Crypto.Internal
import Mammut.Vault

import TestHelpers

operationsTests :: TestTree
operationsTests = testGroup "Mammut.Vault.Operations"
  [ testProperty "writeVersion writes a version that is listed by\
                 \ readVersions" $ property $ do
      vault <- forAll vaultGen
      time  <- forAll utctimeGen
      hash  <- forAll hashGen

      inTestEnv_ $ do
        version  <- writeVersion vault time hash
        versions <- readVersions vault
        versions === [version]

  , testProperty "writePlainObject and readPlainObject are\
                 \ inverses" $ property $ do
      vault    <- forAll vaultGen
      contents <- forAll contentsGen

      inTestEnv_ $ do
        hash <- writePlainObject vault contents
        contents' <- readPlainObject vault hash
        contents' === contents

  , testProperty "writeDirectory and readDirectory are inverses" $ property $ do
      vault     <- forAll vaultGen
      directory <- forAll directoryGen

      inTestEnv_ $ do
        hash <- writeDirectory vault directory
        directory' <- readDirectory vault hash
        directory' === directory
  ]
