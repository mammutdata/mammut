module Mammut.CommandsTest
  ( commandsTests
  ) where

import Prelude hiding (readFile, writeFile)

import Control.Lens

import Mammut.Commands.Backup
import Mammut.Commands.GenerateKey
import Mammut.Commands.Restore
import Mammut.Crypto
import Mammut.FileSystem
import Mammut.Vault

import TestHelpers

commandsTests :: TestTree
commandsTests = testGroup "Mammut.Commands"
  [ generateKeyCommandTests

  , testProperty "backupCommand and restoreCommand are inverses" $ property $ do
      key       <- forAll encryptionKeyGen
      time      <- forAll utctimeGen
      contents1 <- forAll contentsGen
      contents2 <- forAll contentsGen

      inTestEnv_ $ do
        -- FIXME: Generate random file hierarchy
        writeFile "/dir/file1" contents1
        writeFile "/dir/file2" contents2

        version <- backupCommand key "/vault" "/dir" time
        restoreCommand key "/vault" (version ^. versionName) "/restored"

        contents1' <- readFile "/restored/file1"
        contents2' <- readFile "/restored/file2"

        contents1' === contents1
        contents2' === contents2
  ]

generateKeyCommandTests :: TestTree
generateKeyCommandTests = testGroup "generateKeyCommand"
  [ testCase "writes a newly-generated key to a file" $ do
      eRes <- inTestEnv $ do
        generateKeyCommand "/key1"
        generateKeyCommand "/key2"
        key1 <- keyFromFile "/key1"
        key2 <- keyFromFile "/key2"
        return $ key1 /= key2
      eRes @?= Right True

  , testCase "doesn't overwrite existing files" $ do
      eRes <- inTestEnv $ do
        writeFile "/key" "secret"
        generateKeyCommand "/key"
        readFile "/key"
      eRes @?= Right "secret"
  ]
