{-# LANGUAGE TypeApplications #-}
module User.Password
       ( Password (..)
       , PasswordHash (..)
       , hashPassword
       , validatePassword
       ) where

import qualified Crypto.KDF.BCrypt as BCrypt
import Data.Aeson.Options as AO (defaultOptions)
import Data.Aeson.TH (Options(..), deriveJSON)
import Data.ByteString (ByteString)
import Database.Persist.Sql (PersistField)

import Foundation (HasConfig(..))

newtype PasswordHash = PasswordHash { unPasswordHash :: Text }
  deriving (Eq, Show, PersistField)

$(deriveJSON AO.defaultOptions { unwrapUnaryRecords = True } ''PasswordHash)

newtype Password = Password {unPassword :: Text }
  deriving (Show)

$(deriveJSON AO.defaultOptions { unwrapUnaryRecords = True } ''Password)

hashPassword
  :: (HasConfig r, MonadReader r m)
  => Password
  -> m PasswordHash
hashPassword (Password str) = do
  salt <- view cSalt -- TODO: should also use userName as part of salt
  pure . PasswordHash . decodeUtf8 @Text @ByteString
    $ BCrypt.bcrypt 12 (encodeUtf8 @Text @ByteString salt)
    $ encodeUtf8 @Text @ByteString str

validatePassword :: Password -> PasswordHash -> Bool
validatePassword (Password str) (PasswordHash hash) =
  BCrypt.validatePassword (encodeUtf8 @Text @ByteString str) $ encodeUtf8 @Text @ByteString hash
