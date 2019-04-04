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
import Evaluation.Types as E
import Foundation (HasPool(..))
import User.Types as U

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
    UniqueUserName  name
    UniqueUserEmail email
    deriving Show

  Agent sql=agents
    userId         UserId
    supervisorId   UserId                Maybe default=NULL
    services       [E.ServiceTypeValue]  Maybe default=NULL
    branch         BranchId              Maybe default=NULL
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
    name             E.ServiceType      sqltype=text
    value            E.ServiceTypeValue sqltype=text
    createdAt   UTCTime  sqltype=timestamptz sql=created_at default=CURRENT_TIMESTAMP
    updatedAt   UTCTime  sqltype=timestamptz sql=updated_at default=CURRENT_TIMESTAMP
    UniqueServiceValue value
    deriving Show

  Evaluation sql=paremeter
    evaluator      UserId
    agent          UserId
    serviceType    ServiceId
    reason         E.Reason             sqltype=text
    comment        E.Comment     Maybe  sqltype=text
    duration       E.Duration    Maybe  sqltype=int
    customerNumber E.CustomerNumber     sqltype=int
    createdAt   UTCTime  sqltype=timestamptz sql=created_at default=CURRENT_TIMESTAMP
    updatedAt   UTCTime  sqltype=timestamptz sql=updated_at default=CURRENT_TIMESTAMP
    deriving Show

  Parameter sql=paremeter
    name        Name              sqltype=text
    value       E.PValue          sqltype=text
    description E.Description Maybe sqltype=text default=NULL
    serviceType ServiceId
    category    E.Category        sqltype=text
    group       E.Group    Maybe  sqltype=text default=NULL
    weight      E.Weight          sqltype=int  default=NULL
    createdAt   UTCTime  sqltype=timestamptz sql=created_at default=CURRENT_TIMESTAMP
    updatedAt   UTCTime  sqltype=timestamptz sql=updated_at default=CURRENT_TIMESTAMP
    UniqueParameterValue value
    deriving Show

  ParameterScore sql=paremeter_score
    evaluation  EvaluationId
    parameter   ParameterId
    createdAt   UTCTime  sqltype=timestamptz sql=created_at default=CURRENT_TIMESTAMP
    deriving Show
  |]

$(deriveJSON defaultOptions ''User)
$(deriveJSON defaultOptions ''Branch)
$(deriveJSON defaultOptions ''Service)
$(deriveJSON defaultOptions ''Agent)
$(deriveJSON defaultOptions ''Parameter)
$(deriveJSON defaultOptions ''ParameterScore)
$(deriveJSON defaultOptions ''Evaluation)

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
