import Test.Tasty

import Mammut.CommandsTest
import Mammut.CryptoTest
import Mammut.Vault.OperationsTest

main :: IO ()
main = defaultMain $ testGroup "Root"
  [ commandsTests, cryptoTests, operationsTests ]
