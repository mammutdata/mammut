import Test.Tasty

import Mammut.CryptoTest
import Mammut.Vault.OperationsTest

main :: IO ()
main = defaultMain $ testGroup "Root"
  [ cryptoTests, operationsTests ]
