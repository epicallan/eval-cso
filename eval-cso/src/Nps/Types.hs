module Nps.Types
        ( Telephone (..)
        , WaitTime (..)
        , TouchPoint (..)
        , RatingReason (..)
        , Rating (..)
        , CRMCaptureReason (..)
        , FrontLineRatingReason (..)
        , BackOfficeReason (..)
        , Reason (..)
        , Duration (..)
        , NpsRecord (..)
        , CreateNps (..)
        , NpsErrors (..)
        ) where

import Data.Aeson.Options as AO (defaultOptions)
import Data.Aeson.TH (Options(..), deriveJSON)
import Data.Time (UTCTime)
import Database.Persist.Sql (PersistField)
import Evaluation.Types (Duration(..), Reason(..))
import User.Types (UserName)


newtype Telephone =Telephone { unTelephone :: Text}
  deriving (Eq, Show, PersistField)

$(deriveJSON AO.defaultOptions { unwrapUnaryRecords = True } ''Telephone)

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

newtype CRMCaptureReason = CRMCaptureReason { unCRMCaptureReason :: Int }
  deriving (Eq, Show, PersistField)

$(deriveJSON AO.defaultOptions { unwrapUnaryRecords = True } ''CRMCaptureReason)

newtype FrontLineRatingReason = FrontLineRatingReason { unFrontLineRatingReason :: Int }
  deriving (Eq, Show, PersistField)

$(deriveJSON AO.defaultOptions { unwrapUnaryRecords = True } ''FrontLineRatingReason)

newtype BackOfficeReason = BackOfficeReason { unBackOfficeReason :: Int }
  deriving (Eq, Show, PersistField)

$(deriveJSON AO.defaultOptions { unwrapUnaryRecords = True } ''BackOfficeReason)

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
  , cnTouchPoint :: TouchPoint
  , cnRating :: Rating
  , cnReason :: Reason
  , cnWaitTime :: WaitTime
  , cnDuration :: Duration
  , cnIssueResolved :: Bool
  , cnFurtherInformationGiven :: Bool
  , cnRatingReason :: Maybe RatingReason
  , cnCrmCaptureCorrect :: Bool
  , cnCrmCaptureReason :: Maybe CRMCaptureReason
  , cnFrontLineRatingReason :: FrontLineRatingReason
  , cnBackOfficeReason :: BackOfficeReason
  } deriving (Show)

$(deriveJSON AO.defaultOptions ''CreateNps)

data NpsRecord = NpsRecord
  { nrCustomerTel :: Maybe Telephone
  , nrEvaluator :: UserName
  , nrAgentName :: UserName
  , nrDate :: UTCTime
  , nrTouchPoint :: TouchPoint
  , nrRating :: Rating
  , nrReason :: Reason
  , nrWaitTime :: WaitTime
  , nrDuration :: Duration
  , nrIssueResolved :: Bool
  , nrFurtherInformationGinven :: Bool
  , nrRatingReason :: Maybe RatingReason
  , nrCrmCaptureCorrect :: Bool
  , nrCrmCaptureReason :: Maybe CRMCaptureReason
  , nrFrontLineRatingReason :: FrontLineRatingReason
  , nrBackOfficeReason :: BackOfficeReason
  , nrCreatedAt :: UTCTime
  } deriving (Show)

$(deriveJSON AO.defaultOptions ''NpsRecord)
