import Test.Tasty

import Mammut.CryptoTest
import Mammut.OperationsTest

import TestHelpers

main :: IO ()
main = defaultMain $ testGroup "Root"
  [ cryptoTests, operationsTests ]
