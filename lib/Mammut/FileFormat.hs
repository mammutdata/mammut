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
import           Control.Monad.Except

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

writeVersion :: Version -> BS.ByteString
writeVersion = BS.pack . view versionHash

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

writeDirectory :: Key -> Directory -> IO (Either MammutError BS.ByteString)
writeDirectory key directory = runExceptT $ do
  ls <- forM (directory ^. directoryItems) $ \item -> do
    encName <- fmap (convertToBase Base16 . BSL.toStrict)
               . ExceptT . encryptFile key . BSL.pack $ item ^. itemName
    let typ = case item ^. itemType of
          PlainObject     -> " p "
          DirectoryObject -> " d "
    return $ encName <> typ <> BS.pack (item ^. itemHash)
  return $ BS.unlines ls
