{-# LANGUAGE EmptyDataDecls, QuasiQuotes #-}

module Model
  ( -- auto generated persistent Field types e.g UserName
    EntityField (..)
  , Branch
  , BranchId
  , User (..)
  , UserId

  , doMigrations
  , runInDb
  ) where

import Data.Aeson.TH (defaultOptions, deriveJSON)
import Data.Time (UTCTime)
import Database.Persist.Sql (EntityField, SqlPersistT, runMigration, runSqlPool)
import Database.Persist.TH
  (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)
import Servant.Auth.Server (ToJWT, FromJWT)

import Common.Types (Name)
import Foundation (HasPool(..))
import User.Types as U (Email, Role, PasswordHash)

share
  [mkPersist sqlSettings
  , mkMigrate "migrateAll"
  ] [persistLowerCase|
  User sql=users
    name           Name           sqltype=text
    email          U.Email        sqltype=text
    role           U.Role         sqltype=text
    password       U.PasswordHash   sqltype=text
    createdAt      UTCTime  sqltype=timestamptz sql=created_at default=CURRENT_TIMESTAMP
    updatedAt      UTCTime  sqltype=timestamptz sql=updated_at default=CURRENT_TIMESTAMP
    UniqueEmail    email
    UniqueName     name
    deriving Show

  Branch sql=branch
    name        Text sqltype=text
    createdAt   UTCTime  sqltype=timestamptz sql=created_at default=CURRENT_TIMESTAMP
    updatedAt   UTCTime  sqltype=timestamptz sql=updated_at default=CURRENT_TIMESTAMP
   deriving Show

  |]

$(deriveJSON defaultOptions ''Branch)
$(deriveJSON defaultOptions ''User)

instance ToJWT User
instance FromJWT User

-- makeLensesWith camelCaseFields ''User

-- we should be able to run multiple migrations
doMigrations :: SqlPersistT IO ()
doMigrations = runMigration migrateAll

runInDb
  :: forall r m a. (MonadIO m, MonadReader r m, HasPool r)
  => SqlPersistT IO a
  -> m a
runInDb query = do
    cPool <- view pool
    liftIO $ runSqlPool query cPool
