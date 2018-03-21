import Test.Tasty

import Mammut.CryptoTest
import Mammut.OperationsTest

main :: IO ()
main = defaultMain $ testGroup "Root"
  [ cryptoTests, operationsTests ]
