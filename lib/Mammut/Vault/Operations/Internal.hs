module Mammut.Vault.Operations.Internal where

import           Prelude hiding (readFile, writeFile)

import           Control.Eff
import           Control.Eff.Exception (Exc, throwError, catchError)
import           Control.Lens
import           Control.Monad

import           Data.List (find, sort)
import           Data.Time (UTCTime, defaultTimeLocale, formatTime)
import qualified Data.Attoparsec.ByteString.Lazy as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

import           System.FilePath

import           Mammut.Crypto
import           Mammut.Errors
import           Mammut.FileSystem
import           Mammut.Vault.FileFormat
import           Mammut.Vault.Types

data VaultOp a where
  ReadVersions     :: Vault -> VaultOp [Version]
  ReadPlainObject  :: Vault -> ObjectHash -> VaultOp BSL.ByteString
  ReadDirectory    :: Vault -> ObjectHash -> VaultOp Directory
  WriteVersion     :: Vault -> UTCTime -> ObjectHash -> VaultOp Version
  WritePlainObject :: Vault -> BSL.ByteString -> VaultOp ObjectHash
  WriteDirectory   :: Vault -> Directory      -> VaultOp ObjectHash

-- FIXME: Make those exceptions catchable by the program

readVersions_ :: (Member (Exc MammutError) r, Member FileSystem r)
              => Vault -> Eff r [Version]
readVersions_ vault = do
  let dir = vault ^. vaultLocation </> "versions"
  names <- listDirectory dir `catchError` \case
    FileNotFound _ -> return []
    e -> throwError e

  forM (sort names) $ \name -> do
    let path = dir </> name
    contents <- readFile path
    either (throwError . UnreadableVersion path) return $ A.eitherResult $
      A.parse (parseSigned (vault ^. vaultKey) (parseVersion name)) contents

readPlainObject_ :: (Member (Exc MammutError) r, Member FileSystem r)
                 => Vault -> ObjectHash -> Eff r BSL.ByteString
readPlainObject_ vault hash = do
  let (dir, fname) = objectLocation vault hash
      path = dir </> fname
  encrypted <- readFile path
  case decryptFile (vault ^. vaultKey) encrypted of
    Left err -> throwError err
    Right contents
      | hashFile contents == hash -> return contents
      | otherwise -> throwError $ CorruptedFile path

readDirectory_ :: (Member (Exc MammutError) r, Member FileSystem r)
               => Vault -> ObjectHash -> Eff r Directory
readDirectory_ vault hash = do
  let (dir, fname) = objectLocation vault hash
      path = dir </> fname
      key  = vault ^. vaultKey
  contents <- readFile path
  either (throwError . UnreadableDirectory path) return $ A.eitherResult $
    A.parse (parseSigned key (parseDirectory key)) contents

writeVersion_ :: (Member (Exc MammutError) r, Member FileSystem r)
              => Vault -> UTCTime -> ObjectHash -> Eff r Version
writeVersion_ vault time hash = do
    versions <- readVersions_ vault
    let f v = v ^. versionTime == time && v ^. versionHash == hash
    case find f versions of
      Just version -> return version
      Nothing -> do
        let base    = formatTime defaultTimeLocale "%Y%m%d%H%M" time
            name    = findName (map (view versionName) versions) base Nothing
            path    = vault ^. vaultLocation </> "versions" </> name
            version = Version name time hash
        writeFile path $ writeSigned (vault ^. vaultKey) $ writeVersion version
        return version

  where
    findName :: [String] -> String -> Maybe Int -> String
    findName takens base = \case
      Nothing | base `elem` takens -> findName takens base (Just 0)
              | otherwise -> base
      Just c -> let name = base ++ '.' : show c
                in if name `elem` takens
                     then findName takens base (Just (c+1))
                     else name

writePlainObject_ :: (Member (Exc MammutError) r, Member FileSystem r)
                  => Vault -> BS.ByteString -> BSL.ByteString
                  -> Eff r ObjectHash
writePlainObject_ vault iv contents = do
  let hash         = hashFile contents
      (dir, fname) = objectLocation vault hash
      path         = dir </> fname
  mType <- checkFile path
  case mType of
    Just _ -> return hash
    Nothing -> do
      case encryptFile (vault ^. vaultKey) iv contents of
        Left err -> throwError err
        Right enc -> do
          writeFile path enc
          return hash

writeDirectory_ :: (Member (Exc MammutError) r, Member FileSystem r)
                => Vault -> BS.ByteString -> Directory -> Eff r ObjectHash
writeDirectory_ vault iv directory = do
  let key = vault ^. vaultKey
  case writeDirectory key iv directory of
    Left err -> throwError err
    Right contents -> do
      let bsl  = writeSigned key contents
          hash = hashFile bsl
          (dir, fname) = objectLocation vault hash
          path = dir </> fname
      writeFile path bsl
      return hash

objectLocation :: Vault -> ObjectHash -> (FilePath, FilePath)
objectLocation vault hash =
  let (prefix, fname) = splitAt 2 hash
      dir = vault ^. vaultLocation </> "objects" </> prefix
  in (dir, fname)
