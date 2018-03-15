module Mammut.Crypto.Internal where

import           GHC.Generics (Generic)

import           Control.Lens

import qualified Data.ByteString as BS

-- | Signed entity. This type is use when parsing/writing something to a file.
newtype Signed a = Signed { _fromSigned :: a } deriving (Eq, Show, Generic)

makeLenses ''Signed

newtype Key = Key BS.ByteString deriving Show
