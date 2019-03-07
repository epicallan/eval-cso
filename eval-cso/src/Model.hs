{-# LANGUAGE EmptyDataDecls, QuasiQuotes #-}
module Model
  ( module Model
  ) where

import Data.Aeson.TH (defaultOptions, deriveJSON)
import Data.Time (UTCTime)
import Database.Esqueleto
import Database.Persist.Sql (SqlPersistT, runMigration, runSqlPool)
import Database.Persist.TH
  (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)
import Servant.Auth.Server (FromJWT, ToJWT)

import Common.Types (Name)
import Foundation (HasPool(..))
import User.Types as U (Email, PasswordHash, Role)

share
  [ mkPersist sqlSettings
  , mkMigrate "migrateAll"
  ] [persistLowerCase|
  User sql=users
    name           Name           sqltype=text
    email          U.Email        sqltype=text
    role           U.Role         sqltype=text
    password       U.PasswordHash sqltype=text
    createdAt      UTCTime  sqltype=timestamptz sql=created_at default=CURRENT_TIMESTAMP
    updatedAt      UTCTime  sqltype=timestamptz sql=updated_at default=CURRENT_TIMESTAMP
    UniqueUser     email name password
    deriving Show

  Agent sql=agents
    userId         UserId
    supervisorId   UserId       Maybe default=NULL
    services       [ServiceId]  Maybe default=NULL
    branch         BranchId     Maybe default=NULL
    createdAt      UTCTime      sqltype=timestamptz sql=created_at default=CURRENT_TIMESTAMP
    updatedAt      UTCTime      sqltype=timestamptz sql=updated_at default=CURRENT_TIMESTAMP
    UniqueAgent    userId
    deriving Show

  Branch sql=branch
    name        Name     sqltype=text
    createdAt   UTCTime  sqltype=timestamptz sql=created_at default=CURRENT_TIMESTAMP
    updatedAt   UTCTime  sqltype=timestamptz sql=updated_at default=CURRENT_TIMESTAMP
    UniqueBranch name
    deriving Show

  Service sql=services
    name        Name     sqltype=text
    createdAt   UTCTime  sqltype=timestamptz sql=created_at default=CURRENT_TIMESTAMP
    updatedAt   UTCTime  sqltype=timestamptz sql=updated_at default=CURRENT_TIMESTAMP
    UniqueService name
    deriving Show
  |]

$(deriveJSON defaultOptions ''User)
$(deriveJSON defaultOptions ''Branch)
$(deriveJSON defaultOptions ''Service)
$(deriveJSON defaultOptions ''Agent)

instance ToJWT User
instance FromJWT User

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
