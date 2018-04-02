module Mammut.Options
  ( Command(..)
  , Options(..)
  , getCommandOptions
  ) where

import Data.Monoid

import Options.Applicative

import Mammut.Crypto

data Command
  = GenerateKey
  | Backup FilePath
  | Restore String FilePath

data Options = Options
  { optsKeyPath   :: Maybe FilePath
  , optsVaultPath :: Maybe FilePath
  }

instance Monoid Options where
  mempty = Options Nothing Nothing
  mappend a b =
    let choose f = maybe (f a) Just (f b)
    in Options
         { optsKeyPath   = choose optsKeyPath
         , optsVaultPath = choose optsVaultPath
         }

optionsParser :: Parser Options
optionsParser = Options
    <$> maybeOption (short 'k' <> long "key" <> metavar "KEY_FILE"
                     <> help "Path to the file containing the AES256 encryption\
                             \ key.")
    <*> maybeOption (short 'v' <> long "vault" <> metavar "VAULT_PATH"
                     <> help "Path to the vault.")

  where
    maybeOption :: Mod OptionFields String -> Parser (Maybe String)
    maybeOption mods =
      (\s -> if null s then Nothing else Just s)
      <$> strOption (mods <> value "")

commandParser :: Parser (Command, Options)
commandParser = subparser
  (command "generate-key"
     (info (helper <*> withOptions <*> pure GenerateKey)
           (progDesc "Generate encryption key."))
   <> command "backup"
        (info (helper <*> withOptions <*> (Backup <$> directoryArgument))
              (progDesc "Back up the contents of a directory into the vault."))
   <> command "restore"
        (info (helper <*> withOptions
                      <*> (Restore <$> versionArgument <*> directoryArgument))
              (progDesc "Restore a version from the vault to some directory.")))

directoryArgument :: Parser FilePath
directoryArgument = strArgument $ metavar "DIRECTORY"

versionArgument :: Parser String
versionArgument = strArgument $ metavar "VERSION_NAME"

withOptions :: Parser (a -> (a, Options))
withOptions = (\o x -> (x, o)) <$> optionsParser

commandOptionsParser :: Parser (Command, Options)
commandOptionsParser =
  (\o (c, o') -> (c, o <> o')) <$> optionsParser <*> commandParser

getCommandOptions :: IO (Command, Options)
getCommandOptions = execParser $ info (helper <*> commandOptionsParser) $
  fullDesc
  <> progDesc "Backup tool, see https://www.mammutdata.com"
  <> footer "This program is licensed under the BSD-3 license."
