module Mammut.Operations.Internal where

import           Prelude hiding (readFile, writeFile)

import           Control.Eff
import           Control.Eff.Exception (Exc, throwError, catchError)
import           Control.Lens
import           Control.Monad

import           Data.List (sort)
import           Data.Maybe (catMaybes)
import qualified Data.Attoparsec.ByteString.Lazy as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

import           System.FilePath

import           Mammut.Crypto
import           Mammut.Errors
import           Mammut.FileFormat
import           Mammut.FileSystem
import           Mammut.Vault

data Mammut a where
  ReadVault        :: Key   -> FilePath   -> Mammut Vault
  ReadPlainObject  :: Vault -> ObjectHash -> Mammut BSL.ByteString
  ReadDirectory    :: Vault -> ObjectHash -> Mammut (Signed Directory)
  WriteVault       :: Vault -> Mammut ()
  WritePlainObject :: Vault -> BSL.ByteString -> Mammut ObjectHash
  WriteDirectory   :: Vault -> Directory      -> Mammut ObjectHash

readVault_ :: (Member (Exc MammutError) r, Member FileSystem r)
           => Key -> FilePath -> Eff r Vault
readVault_ key path = do
  let dir = path </> "versions"
  names <- listDirectory dir `catchError` \case
    FileNotFound _ -> return []
    e -> throwError e
  versions <- fmap catMaybes . forM (sort names) $ \name -> do
    contents <- readFile $ dir </> name
    return $ A.maybeResult $
      A.parse (parseSigned key (parseVersion name)) contents
  return $ Vault key path (map fromSigned versions)

readPlainObject_ :: (Member (Exc MammutError) r, Member FileSystem r)
                 => Vault -> ObjectHash
                 -> Eff r (Either MammutError BSL.ByteString)
readPlainObject_ vault hash = do
  let (dir, fname) = objectLocation vault hash
      path = dir </> fname
  encrypted <- readFile path
  let eContents = decryptFile (vault ^. vaultKey) encrypted
  return $ case eContents of
    Left _ -> eContents
    Right contents
      | hashFile contents == hash -> eContents
      | otherwise -> Left $ CorruptedFile path

readDirectory_ :: (Member (Exc MammutError) r, Member FileSystem r)
               => Vault -> ObjectHash
               -> Eff r (Either MammutError (Signed Directory))
readDirectory_ vault hash = do
  let (dir, fname) = objectLocation vault hash
      path = dir </> fname
      key  = vault ^. vaultKey
  -- FIXME: catch IO errors
  contents <- readFile path
  let eRes = A.eitherResult $
        A.parse (parseSigned key (parseDirectory key)) contents
  return $ either (Left . UnreadableDirectory) Right eRes

writeVault_ :: (Member (Exc MammutError) r, Member FileSystem r)
            => Vault -> Eff r ()
writeVault_ vault = do
  let dir = vault ^. vaultLocation </> "versions"
  forM_ (vault ^. vaultVersions) $ \version -> do
    let name = getVersionFilePath version
    writeFile (dir </> name) $
      writeSigned (vault ^. vaultKey) $ writeVersion version

writePlainObject_ :: (Member (Exc MammutError) r, Member FileSystem r)
                  => Vault -> BS.ByteString
                  -> BSL.ByteString -> Eff r (Either MammutError ObjectHash)
writePlainObject_ vault iv contents = do
  let hash         = hashFile contents
      (dir, fname) = objectLocation vault hash
      path         = dir </> fname
  mType <- checkFile path
  case mType of
    Just _ -> return $ Right hash
    Nothing -> do
      case encryptFile (vault ^. vaultKey) iv contents of
        Left err -> return $ Left err
        Right enc -> do
          writeFile path enc
          return $ Right hash

writeDirectory_ :: (Member (Exc MammutError) r, Member FileSystem r)
                => Vault -> BS.ByteString -> Directory
                -> Eff r (Either MammutError ObjectHash)
writeDirectory_ vault iv directory = do
  let key = vault ^. vaultKey
  case writeDirectory key iv directory of
    Left err -> return $ Left err
    Right contents -> do
      let bsl  = writeSigned key contents
          hash = hashFile bsl
          (dir, fname) = objectLocation vault hash
          path = dir </> fname
      writeFile path bsl
      return $ Right hash

objectLocation :: Vault -> ObjectHash -> (FilePath, FilePath)
objectLocation vault hash =
  let (prefix, fname) = splitAt 2 hash
      dir = vault ^. vaultLocation </> "objects" </> prefix
  in (dir, fname)
