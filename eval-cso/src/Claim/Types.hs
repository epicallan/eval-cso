module Claim.Types
        ( Comment (..)
        , AllParametersMet
        , HasClaimRecord (..)
        , HasCreateClaim (..)
        , ClaimRecord (..)
        , CreateClaim (..)
        , ClaimTypeRecord (..)
        , ClaimTypeName (..)
        , ClaimTypeValue (..)
        , WorkflowNumber (..)
        , ClaimErrors (..)
        , Score (..)
        ) where

import Data.Aeson.Options as AO (defaultOptions)
import Data.Aeson.TH (Options(..), deriveJSON)
import Data.Time (UTCTime)
import Database.Persist.Sql (PersistField)
import Lens.Micro.Platform (makeClassy)

import Evaluation.Types (Comment(..), Score(..))
import User.Types (UserName)

type AllParametersMet = Bool

newtype ClaimTypeName = ClaimTypeName { unClaimTypeName :: Text }
  deriving (Eq, Show, PersistField)

$(deriveJSON AO.defaultOptions { unwrapUnaryRecords = True } ''ClaimTypeName)


newtype ClaimTypeValue = ClaimTypeValue { unClaimTypeValue :: Text }
  deriving (Eq, Show, PersistField)

$(deriveJSON AO.defaultOptions { unwrapUnaryRecords = True } ''ClaimTypeValue)

newtype WorkflowNumber = WorkflowNumber { unWorkflowNumber :: Int }
  deriving (Eq, Show, PersistField)

$(deriveJSON AO.defaultOptions { unwrapUnaryRecords = True } ''WorkflowNumber)

data ClaimErrors =
    ClaimTypeNotFound ClaimTypeValue
  | UserNameNotFound UserName
  | ActionIsForEvaluators UserName
  | ActionIsForAdmins UserName
  deriving Show

instance Exception ClaimErrors

data CreateClaim = CreateClaim
  { _ccAllParametersMet :: AllParametersMet
  , _ccAgentName :: UserName
  , _ccWorkflowNumber :: WorkflowNumber
  , _ccComment :: Maybe Comment
  , _ccClaimType :: ClaimTypeValue
  } deriving (Show)

$(deriveJSON AO.defaultOptions ''CreateClaim)
makeClassy ''CreateClaim

data ClaimRecord = ClaimRecord
  { _crScore :: Score
  , _crEvaluator :: UserName
  , _crAgentName :: UserName
  , _crWorkflowNumber :: WorkflowNumber
  , _crComment :: Maybe Comment
  , _crClaimType :: ClaimTypeName
  , _crDate :: UTCTime
  } deriving (Show)

$(deriveJSON AO.defaultOptions ''ClaimRecord)
makeClassy ''ClaimRecord

data ClaimTypeRecord = ClaimTypeRecord
  { _ctrName :: ClaimTypeName
  , _ctrValue :: ClaimTypeValue
  } deriving (Show)

$(deriveJSON AO.defaultOptions ''ClaimTypeRecord)
