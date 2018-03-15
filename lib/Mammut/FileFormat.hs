module Mammut.FileFormat
  ( parseVersion
  , writeVersion
  , getVersionFilePath
  ) where

import           Prelude hiding (take, takeWhile)

import           Control.Applicative (many)
import           Control.Lens

import           Data.Attoparsec.ByteString.Char8
import           Data.Char (isHexDigit)
import           Data.Time (parseTimeM, defaultTimeLocale, formatTime)
import qualified Data.ByteString.Char8 as BS

import           Mammut.Vault

parseVersion :: FilePath -> Parser Version
parseVersion path = do
  time <- parseTimeM True defaultTimeLocale "%Y%m%d%H%M%S" path

  hash <- BS.unpack <$> takeWhile isHexDigit
  _ <- many endOfLine
  endOfInput

  return $ Version time hash

writeVersion :: Version -> BS.ByteString
writeVersion = BS.pack . view versionHash

getVersionFilePath :: Version -> FilePath
getVersionFilePath =
  formatTime defaultTimeLocale "%Y%m%d%H%M%S" . view versionTime
