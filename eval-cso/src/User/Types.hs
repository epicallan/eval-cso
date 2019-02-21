module User.Types
        ( Email (..)
        , Role (..)
        , UserStorageErrors(..)
        ) where

import Data.Aeson.Options as AO (defaultOptions)
import Data.Aeson.TH (Options(..), deriveJSON)
import Database.Persist.Sql (PersistField)
import Database.Persist.TH (derivePersistField)

import Common.Types (Id)

data Role =
    Admin
  | Member
  | Guest
  deriving (Eq, Show, Read)

$(deriveJSON AO.defaultOptions ''Role)
derivePersistField "Role"

newtype Email = Email {unEmail :: Text}
  deriving (Eq, Show, PersistField)

$(deriveJSON AO.defaultOptions { unwrapUnaryRecords = True } ''Email)

data UserStorageErrors =
    UserEmailNotFound Email
  | UserNotFound Id
  deriving Show

instance Exception UserStorageErrors

