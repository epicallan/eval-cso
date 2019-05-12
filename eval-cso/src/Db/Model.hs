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

import qualified Agent.Types as A
import qualified Claim.Types as C
import qualified Evaluation.Types as E
import Foundation (HasPool(..))
import qualified Nps.Types as N
import qualified User.Types as U

share
  [ mkPersist sqlSettings
  , mkMigrate "migrateAll"
  ] [persistLowerCase|
  User sql=users
    name           U.UserName     sqltype=text
    fullName       U.FullName    sqltype=text
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
    name        A.BranchName     sqltype=text
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
    details        E.Details     Maybe  sqltype=text
    customerTel    E.Telephone          sqltype=text
    createdAt   UTCTime  sqltype=timestamptz sql=created_at default=CURRENT_TIMESTAMP
    updatedAt   UTCTime  sqltype=timestamptz sql=updated_at default=CURRENT_TIMESTAMP
    deriving Show

  ClaimType sql=claim_type
    name        C.ClaimTypeName  sqltype=text
    value       C.ClaimTypeValue sqltype=text
    createdAt   UTCTime  sqltype=timestamptz sql=created_at default=CURRENT_TIMESTAMP
    updatedAt   UTCTime  sqltype=timestamptz sql=updated_at default=CURRENT_TIMESTAMP
    UniqueClaimTypeValue value
    deriving Show

  Claim sql=claim
    evaluator        UserId
    agent            UserId
    allParametersMet C.AllParametersMet
    comment          E.Comment  Maybe   sqltype=text default=Null
    workflowNumber   C.WorkflowNumber   sqltype=int
    claimType        ClaimTypeId
    details          E.Details    Maybe  sqltype=text
    UniqueWorkflowNumber workflowNumber
    createdAt   UTCTime  sqltype=timestamptz sql=created_at default=CURRENT_TIMESTAMP
    updatedAt   UTCTime  sqltype=timestamptz sql=updated_at default=CURRENT_TIMESTAMP
    deriving Show

  Nps sql=nps
   evaluator                UserId
   customerTel              E.Telephone          Maybe sqltype=int default Null
   agent                    UserId
   date                     UTCTime                    sqltype=timestamptz
   touchPoint               N.TouchPoint               sqltype=text
   reason                   E.Reason                   sqltype=text
   waitTime                 N.WaitTime                 sqltype=int
   duration                 N.Duration                 sqltype=int
   issueResolved            Bool
   furtherInformationGiven  Bool
   rating                   N.Rating                    sqltype=int
   ratingReason             N.RatingReason        Maybe sqltype=text default=Null
   crmCaptureCorrect        N.CaptureCorrectState       sqltype=text
   crmCaptureReason         N.CRMCaptureReason    Maybe sqltype=text default=Null
   frontLineRatingReasons   [N.FrontLineRatingReason]
   backOfficeReasons        [N.BackOfficeReason]
   createdAt   UTCTime  sqltype=timestamptz sql=created_at default=CURRENT_TIMESTAMP
   updatedAt   UTCTime  sqltype=timestamptz sql=updated_at default=CURRENT_TIMESTAMP
   deriving Show

  Parameter sql=paremeter
    name        E.ParaName           sqltype=text
    value       E.Paravalue          sqltype=text
    description E.Description Maybe  sqltype=text default=NULL
    serviceType ServiceId
    category    E.Category           sqltype=text
    group       E.Group    Maybe     sqltype=text default=NULL
    weight      E.Weight             sqltype=int  default=NULL
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

$(deriveJSON defaultOptions ''Claim)
$(deriveJSON defaultOptions ''ClaimType)
$(deriveJSON defaultOptions ''User)
$(deriveJSON defaultOptions ''Branch)
$(deriveJSON defaultOptions ''Service)
$(deriveJSON defaultOptions ''Agent)
$(deriveJSON defaultOptions ''Parameter)
$(deriveJSON defaultOptions ''ParameterScore)
$(deriveJSON defaultOptions ''Evaluation)

instance ToJWT User
instance FromJWT User

type CanDb m r = (MonadIO m, MonadReader r m, HasPool r, MonadTime m)

doMigrations :: SqlPersistT IO ()
doMigrations = runMigration migrateAll

runMigrations :: CanDb m r => m ()
runMigrations = runInDb doMigrations

runInDb
  :: CanDb m r
  => SqlPersistT IO a
  -> m a
runInDb query = do
    cPool <- view pool
    liftIO $ runSqlPool query cPool
