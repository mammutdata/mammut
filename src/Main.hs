import Control.Eff
import Control.Eff.Exception (Exc, runError)
import Control.Eff.Lift (Lift, lift, runLift)
import Control.Lens

import Data.Maybe (fromMaybe)
import Data.Time (getCurrentTime)

import System.Directory (getHomeDirectory)
import System.Exit
import System.FilePath ((</>))
import System.IO

import Mammut.Commands.Backup
import Mammut.Commands.GenerateKey
import Mammut.Commands.Restore
import Mammut.Crypto
import Mammut.Errors
import Mammut.FileSystem
import Mammut.Options
import Mammut.Vault

main :: IO ()
main = do
  (command, Options{..}) <- getCommandOptions

  home <- getHomeDirectory
  let keyFile   = fromMaybe (home </> ".mammut" </> "key") optsKeyPath
      vaultPath = fromMaybe "/var/backup"                  optsVaultPath

  now <- getCurrentTime

  -- FIXME: Check permissions (600) on key file

  eRes <- runMammut $ case command of
    GenerateKey -> do
      generateKeyCommand keyFile
    Backup path -> do
      key <- keyFromFile keyFile
      version <- backupCommand key vaultPath path now
      lift $ putStrLn $ "New version in the vault: " ++ version ^. versionName
    Restore name path -> do
      key <- keyFromFile keyFile
      restoreCommand key vaultPath name path

  case eRes of
    Left err -> do
      hPutStrLn stderr $ errorMessage err
      exitFailure
    Right () -> return ()

runMammut :: Eff '[VaultOp, FileSystem, Exc MammutError, Lift IO] a
          -> IO (Either MammutError a)
runMammut = runLift . runError . runFileSystem . runVaultOp
