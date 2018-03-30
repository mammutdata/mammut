module Mammut.FileFormat
  ( parseVersion
  , writeVersion
  , getVersionFilePath
  , parseDirectory
  , writeDirectory
  ) where

import           Prelude hiding (take, takeWhile)

import           Control.Applicative (many, (<|>))
import           Control.Lens
import           Control.Monad

import           Data.Attoparsec.ByteString.Char8
import           Data.ByteArray.Encoding ( convertFromBase, convertToBase
                                         , Base(Base16) )
import           Data.Char (isHexDigit)
import           Data.Monoid
import           Data.Time (parseTimeM, defaultTimeLocale, formatTime)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL

import           Mammut.Crypto
import           Mammut.Errors
import           Mammut.Vault

parseVersion :: FilePath -> Parser Version
parseVersion path = do
  time <- parseTimeM True defaultTimeLocale "%Y%m%d%H%M%S" path

  hash <- BS.unpack <$> takeWhile isHexDigit
  _ <- many endOfLine
  endOfInput

  return $ Version time hash

writeVersion :: Version -> BSL.ByteString
writeVersion = BSL.pack . view versionHash

getVersionFilePath :: Version -> FilePath
getVersionFilePath =
  formatTime defaultTimeLocale "%Y%m%d%H%M%S" . view versionTime

parseDirectory :: Key -> Parser Directory
parseDirectory key = do
  items <- many $ do
    encodedName <- takeWhile1 isHexDigit
    typ         <- (string " p " >> return PlainObject)
                   <|> (string " d " >> return DirectoryObject)
    hash        <- BS.unpack <$> takeWhile1 isHexDigit
    many endOfLine

    encryptedName <- either fail return $ convertFromBase Base16 encodedName
    case decryptFile key $ BSL.fromStrict encryptedName of
      Left  err  -> fail $ show err
      Right name -> return $ DirectoryItem (BSL.unpack name) typ hash

  return $ Directory items

writeDirectory :: Key -> BS.ByteString -> Directory
               -> Either MammutError BSL.ByteString
writeDirectory key firstIV directory = do
    ls <- forMWith (directory ^. directoryItems) firstIV $ \item iv -> do
      encryptedName <-
        encryptFile key iv $ BSL.pack $ item ^. itemName
      let encodedName = BSL.fromStrict . convertToBase Base16 . BSL.toStrict $
            encryptedName
          typ = case item ^. itemType of
            PlainObject     -> " p "
            DirectoryObject -> " d "
          iv' = nextIV iv encryptedName
      return (encodedName <> typ <> BSL.pack (item ^. itemHash), iv')
    return $ BSL.intercalate "\n" ls

  where
    -- FIXME: Make it use an accumulator
    forMWith :: Monad m => [a] -> b -> (a -> b -> m (c, b)) -> m [c]
    forMWith [] _ _ = return []
    forMWith (x:xs) z f = do
      (y, z') <- f x z
      ys      <- forMWith xs z' f
      return $ y : ys
