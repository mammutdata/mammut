module Mammut.Operations.Internal where

import           Control.Lens
import           Control.Monad

import           Data.List (sort)
import           Data.Maybe (catMaybes)
import qualified Data.Attoparsec.ByteString as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

import           System.Directory
import           System.FilePath

import           Mammut.Crypto
import           Mammut.Errors
import           Mammut.FileFormat
import           Mammut.Vault

data Mammut a where
  ReadVault        :: Key   -> FilePath   -> Mammut Vault
  ReadPlainObject  :: Vault -> ObjectHash -> Mammut BSL.ByteString
  ReadDirectory    :: Vault -> ObjectHash -> Mammut (Signed Directory)
  WriteVault       :: Vault -> Mammut ()
  WritePlainObject :: Vault -> BSL.ByteString -> Mammut ObjectHash
  WriteDirectory   :: Vault -> Directory      -> Mammut ObjectHash

readVaultIO :: Key -> FilePath -> IO Vault
readVaultIO key path = do
  let dir = path </> "versions"
  names <- listDirectory dir
  versions <- fmap catMaybes . forM (sort names) $ \name -> do
    contents <- BS.readFile $ dir </> name
    return $ either (const Nothing) Just $
      A.parseOnly (parseSigned key (parseVersion name)) contents
  return $ Vault key path (map fromSigned versions)

readPlainObjectIO :: Vault -> ObjectHash
                  -> IO (Either MammutError BSL.ByteString)
readPlainObjectIO vault hash = do
  let (dir, fname) = objectLocation vault hash
      path = dir </> fname
  -- FIXME: catch IO errors
  encrypted <- BSL.readFile path
  let eContents = decryptFile (vault ^. vaultKey) encrypted
  return $ case eContents of
    Left _ -> eContents
    Right contents
      | hashFile contents == hash -> eContents
      | otherwise -> Left $ CorruptedFile path

readDirectoryIO :: Vault -> ObjectHash
                -> IO (Either MammutError (Signed Directory))
readDirectoryIO vault hash = do
  let (dir, fname) = objectLocation vault hash
      path = dir </> fname
      key  = vault ^. vaultKey
  -- FIXME: catch IO errors
  contents <- BS.readFile path
  let eRes = A.parseOnly (parseSigned key (parseDirectory key)) contents
  return $ either (Left . UnreadableDirectory) Right eRes

writeVaultIO :: Vault -> IO ()
writeVaultIO vault = do
  let dir = vault ^. vaultLocation </> "versions"
  createDirectoryIfMissing True dir
  forM_ (vault ^. vaultVersions) $ \version -> do
    let name = getVersionFilePath version
    BS.writeFile (dir </> name) $
      writeSigned (vault ^. vaultKey) $ writeVersion version

writePlainObjectIO :: Vault -> BSL.ByteString
                   -> IO (Either MammutError ObjectHash)
writePlainObjectIO vault contents = do
  let hash         = hashFile contents
      (dir, fname) = objectLocation vault hash
      path         = dir </> fname
  check <- doesFileExist path
  if check
    then return $ Right hash
    else do
      createDirectoryIfMissing True dir
      eEncrypted <- encryptFile (vault ^. vaultKey) contents
      case eEncrypted of
        Left err -> return $ Left err
        Right enc -> do
          BSL.writeFile path enc
          return $ Right hash

writeDirectoryIO :: Vault -> Directory -> IO (Either MammutError ObjectHash)
writeDirectoryIO vault directory = do
  let key = vault ^. vaultKey
  eContents <- writeDirectory key directory
  case eContents of
    Left err -> return $ Left err
    Right contents -> do
      let bs   = writeSigned key contents
          hash = hashFile $ BSL.fromStrict bs
          (dir, fname) = objectLocation vault hash
          path = dir </> fname
      createDirectoryIfMissing True dir
      BS.writeFile path bs
      return $ Right hash

objectLocation :: Vault -> ObjectHash -> (FilePath, FilePath)
objectLocation vault hash =
  let (prefix, fname) = splitAt 2 hash
      dir = vault ^. vaultLocation </> "objects" </> prefix
  in (dir, fname)
