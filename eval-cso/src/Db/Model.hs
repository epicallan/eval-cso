{-# LANGUAGE EmptyDataDecls, QuasiQuotes #-}
module Db.Model
  ( module Db.Model
  ) where

import Control.Monad.Time (MonadTime)
import Data.Aeson.TH (defaultOptions, deriveJSON)
import Data.Time (UTCTime)
import Database.Esqueleto
import Database.Persist.Sql (SqlPersistT, runMigration, runSqlPool)
import Database.Persist.TH
  (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)
import Servant.Auth.Server (FromJWT, ToJWT)

import Agent.Types as A
import Evaluation.Types as E
import Foundation (Environment(..), HasEnvironment(..), HasPool(..))
import User.Types as U

share
  [ mkPersist sqlSettings
  , mkMigrate "migrateAll"
  ] [persistLowerCase|
  User sql=users
    name           U.Uname           sqltype=text
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
    name        A.Bname     sqltype=text
    createdAt   UTCTime  sqltype=timestamptz sql=created_at default=CURRENT_TIMESTAMP
    updatedAt   UTCTime  sqltype=timestamptz sql=updated_at default=CURRENT_TIMESTAMP
    UniqueBranchName name
    deriving Show

  Service sql=services
    name             E.ServiceType      sqltype=text
    value            E.ServiceTypeValue sqltype=text
    createdAt   UTCTime  sqltype=timestamptz sql=created_at default=CURRENT_TIMESTAMP
    updatedAt   UTCTime  sqltype=timestamptz sql=updated_at default=CURRENT_TIMESTAMP
    UniqueServiceValue value
    deriving Show

  Evaluation sql=evaluation
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
    name        E.ParaName        sqltype=text
    value       E.Pvalue          sqltype=text
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

type CanDb m r = (MonadIO m, MonadReader r m, HasPool r, HasEnvironment r, MonadTime m)

-- we should be able to run multiple migrations
doMigrations :: SqlPersistT IO ()
doMigrations = runMigration migrateAll

runMigrations :: CanDb m r => m ()
runMigrations = do
  env <- view environment
  case env of
    Production -> pass
    _ -> runInDb doMigrations

runInDb
  :: CanDb m r
  => SqlPersistT IO a
  -> m a
runInDb query = do
    cPool <- view pool
    liftIO $ runSqlPool query cPool
