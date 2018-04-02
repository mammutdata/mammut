module Mammut.Vault.Types where

import GHC.Generics (Generic)

import Data.Time (UTCTime)

import Control.Lens (makeLenses)

import Mammut.Crypto.Internal

-- | Objects can contain data of different types in different formats.
data ObjectType
  = PlainObject     -- ^ The contents of a file as is.
  | DirectoryObject -- ^ A structure describing the contents of a directory.
  deriving (Eq, Show, Generic)

makeLenses ''ObjectType

-- | Objects are named after a hash of their contents.
type ObjectHash = FilePath

-- | An item in a directory, e.g. a file or another directory.
data DirectoryItem = DirectoryItem
  { _itemName :: FilePath   -- ^ Name of the file or directory.
  , _itemType :: ObjectType -- ^ Type and format of the object.
  , _itemHash :: ObjectHash -- ^ Link to the object.
  } deriving (Eq, Show, Generic)

makeLenses ''DirectoryItem

-- | The contents of a directory.
--
-- Directory objects are stored unencrypted in order to know the objects that
-- are linked to by the directory but the names are encrypted. To avoid
-- tampering of a directory object, they are signed. See 'Signed'.
newtype Directory = Directory
  { _directoryItems :: [DirectoryItem] -- ^ Files or subdirectories.
  } deriving (Eq, Show, Generic)

makeLenses ''Directory

-- | A version is simply a link to an object with a timestamp.
--
-- As for directories, these links are stored unencrypted and are signed to
-- avoid tampering. See 'Signed'
data Version = Version
  { _versionName :: String     -- ^ Name identifying the version.
  , _versionTime :: UTCTime    -- ^ Time of the backup.
  , _versionHash :: ObjectHash -- ^ Directory object for the version.
  } deriving (Eq, Show, Generic)

makeLenses ''Version

-- | Directory containing all backed-up versions of some data.
data Vault = Vault
  { _vaultKey      :: Key      -- ^ Encryption key that secures the vault.
  , _vaultLocation :: FilePath -- ^ Absolute path to the vault.
  } deriving (Eq, Show, Generic)

makeLenses ''Vault
