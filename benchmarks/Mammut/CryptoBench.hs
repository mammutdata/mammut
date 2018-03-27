module Mammut.CryptoBench
  ( cryptoBench
  ) where

import           Data.FileEmbed (embedFile)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

import           Criterion

import           Mammut.Crypto.Internal

randomData :: BS.ByteString
randomData = $(embedFile "benchmarks/random.data")

cryptoBench :: Benchmark
cryptoBench = bgroup "Mammut.Crypto"
  [ bench "cfbDecryptLazy . cfbEncryptLazy" $
      let contents = BSL.fromChunks (replicate 100 randomData)
          key      = Key "abcdefghijklmnopqrstuvwxyz012345"
          iv       = "012345abcdefghijklmnopqrstuvwxyz"

          f bs =
            let Right enc = cfbEncryptLazy key iv bs
                Right bs' = cfbDecryptLazy key iv enc
            in bs' == bs

      in nf f contents
  ]
