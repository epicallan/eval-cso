module Nps.Types
        ( Telephone (..)
        , WaitTime (..)
        , TouchPoint (..)
        , RatingReason (..)
        , Rating (..)
        , CRMCaptureReason (..)
        , CrmCaptureCorrect (..)
        , FrontLineRatingReason (..)
        , BackOfficeReason (..)
        , Reason (..)
        , Duration (..)
        , NpsRecord (..)
        , CreateNps (..)
        , NpsErrors (..)
        , BranchName (..)
        ) where

import Data.Aeson.Options as AO (defaultOptions)
import Data.Aeson.TH (Options(..), deriveJSON)
import Data.Time (UTCTime)
import Database.Persist.Sql (PersistField)

import Common.Types (RecordId)
import Evaluation.Types (BranchName(..), Reason(..), Telephone(..))
import User.Types (FullName, UserName)

newtype Duration =Duration { unDuration :: Int}
  deriving (Eq, Show, PersistField)

$(deriveJSON AO.defaultOptions { unwrapUnaryRecords = True } ''Duration)


newtype TouchPoint = TouchPoint { unTouchPoint :: Text }
  deriving (Eq, Show, PersistField)

$(deriveJSON AO.defaultOptions { unwrapUnaryRecords = True } ''TouchPoint)

newtype WaitTime = WaitTime { unWaitTime :: Int }
  deriving (Eq, Show, PersistField)

$(deriveJSON AO.defaultOptions { unwrapUnaryRecords = True } ''WaitTime)

newtype Rating = Rating { unRating :: Int }
  deriving (Eq, Show, PersistField)

$(deriveJSON AO.defaultOptions { unwrapUnaryRecords = True } ''Rating)

newtype RatingReason = RatingReason { unRatingReason :: Text }
  deriving (Eq, Show, PersistField)

$(deriveJSON AO.defaultOptions { unwrapUnaryRecords = True } ''RatingReason)

newtype CRMCaptureReason = CRMCaptureReason { unCRMCaptureReason :: Text }
  deriving (Eq, Show, PersistField)

$(deriveJSON AO.defaultOptions { unwrapUnaryRecords = True } ''CRMCaptureReason)

newtype FrontLineRatingReason = FrontLineRatingReason { unFrontLineRatingReason :: Text }
  deriving (Eq, Show, PersistField)

$(deriveJSON AO.defaultOptions { unwrapUnaryRecords = True } ''FrontLineRatingReason)

newtype BackOfficeReason = BackOfficeReason { unBackOfficeReason :: Text }
  deriving (Eq, Show, PersistField)

$(deriveJSON AO.defaultOptions { unwrapUnaryRecords = True } ''BackOfficeReason)

newtype CrmCaptureCorrect = CrmCaptureCorrect { crmCaptureCorrect :: Text }
  deriving (Eq, Show, PersistField)

$(deriveJSON AO.defaultOptions { unwrapUnaryRecords = True } ''CrmCaptureCorrect)


data NpsErrors =
    NpsTypeNotFound TouchPoint
  | UserNameNotFound UserName
  | ActionIsForEvaluators UserName
  deriving Show

instance Exception NpsErrors

data CreateNps = CreateNps
  { cnCustomerTel :: Maybe Telephone
  , cnAgentName :: UserName
  , cnDate :: UTCTime
  , cnBranch :: BranchName
  , cnRating :: Rating
  , cnReason :: Reason
  , cnWaitTime :: WaitTime
  , cnDuration :: Duration
  , cnIssueResolved :: Bool
  , cnFurtherInformationGiven :: Bool
  , cnRatingReason :: Maybe RatingReason
  , cnCrmCaptureCorrect :: CrmCaptureCorrect
  , cnCrmCaptureReason :: Maybe CRMCaptureReason
  , cnFrontLineRatingReasons :: [FrontLineRatingReason]
  , cnBackOfficeReasons :: [BackOfficeReason]
  } deriving (Show)

$(deriveJSON AO.defaultOptions ''CreateNps)

data NpsRecord = NpsRecord
  { nrCustomerTel :: Maybe Telephone
  , nrEvaluator :: UserName
  , nrAgentName :: UserName
  , nrSupervisor :: Maybe FullName
  , nrBranch :: BranchName
  , nrDate :: UTCTime
  , nrRating :: Rating
  , nrReason :: Reason
  , nrWaitTime :: WaitTime
  , nrDuration :: Duration
  , nrIssueResolved :: Bool
  , nrFurtherInformationGiven :: Bool
  , nrRatingReason :: Maybe RatingReason
  , nrCrmCaptureCorrect :: CrmCaptureCorrect
  , nrCrmCaptureReason :: Maybe CRMCaptureReason
  , nrFrontLineRatingReasons :: [FrontLineRatingReason]
  , nrBackOfficeReasons :: [BackOfficeReason]
  , nrId :: RecordId
  , nrCreatedAt :: UTCTime
  } deriving (Show)

$(deriveJSON AO.defaultOptions ''NpsRecord)
