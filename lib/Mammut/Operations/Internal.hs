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
import           Mammut.FileFormat
import           Mammut.Vault

data MammutRead a where
  ReadVault       :: FilePath   -> MammutRead Vault
  ReadPlainObject :: ObjectHash -> MammutRead BSL.ByteString
  ReadDirectory   :: ObjectHash -> MammutRead (Signed Directory)

data MammutWrite a where
  WriteVault       :: FilePath   -> Vault            -> MammutWrite ()
  WritePlainObject :: ObjectHash -> BSL.ByteString   -> MammutWrite ()
  WriteDirectory   :: ObjectHash -> Signed Directory -> MammutWrite ()

readVaultIO :: Key -> FilePath -> IO Vault
readVaultIO key path = do
  let dir = path </> "versions"
  names <- listDirectory dir
  versions <- fmap catMaybes . forM (sort names) $ \name -> do
    contents <- BS.readFile $ dir </> name
    return $ either (const Nothing) Just $
      A.parseOnly (parseSigned key (parseVersion name)) contents
  return $ Vault path versions

writeVaultIO :: Key -> Vault -> IO ()
writeVaultIO key vault = do
  let dir = vault ^. vaultLocation </> "versions"
  createDirectoryIfMissing True dir
  forM_ (vault ^. vaultVersions) $ \signedVersion -> do
    let name = getVersionFilePath $ signedVersion ^. fromSigned
    BS.writeFile (dir </> name) $ writeSigned key writeVersion signedVersion
