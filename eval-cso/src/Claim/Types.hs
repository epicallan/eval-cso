module Claim.Types
        ( Comment (..)
        , AllParametersMet
        , HasClaimRecord (..)
        , HasCreateClaim (..)
        , HasCreateClaimTypes (..)
        , ClaimRecord (..)
        , CreateClaim (..)
        , CreateClaimTypes (..)
        , ClaimTypeName (..)
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
import User.Types (UserName, UserResponse)

type AllParametersMet = Bool

newtype ClaimTypeName = ClaimTypeName { unClaimTypeName :: Text }
  deriving (Eq, Show, PersistField)

$(deriveJSON AO.defaultOptions { unwrapUnaryRecords = True } ''ClaimTypeName)

newtype WorkflowNumber = WorkflowNumber { unWorkflowNumber :: Text }
  deriving (Eq, Show, PersistField)

$(deriveJSON AO.defaultOptions { unwrapUnaryRecords = True } ''WorkflowNumber)

data ClaimErrors =
    ClaimTypeNotFound ClaimTypeName
  | UserNameNotFound UserName
  | ActionIsForEvaluators UserName
  | ActionIsForAdmins UserName
  deriving Show

instance Exception ClaimErrors

data CreateClaim = CreateClaim
  { _ccAllParemetersMet :: AllParametersMet
  , _ccAgentName :: UserName
  , _ccWorkflowNumber :: WorkflowNumber
  , _ccComment :: Maybe Comment
  , _ccClaimType :: ClaimTypeName
  } deriving (Show)

$(deriveJSON AO.defaultOptions ''CreateClaim)
makeClassy ''CreateClaim

data ClaimRecord = ClaimRecord
  { _crScore :: Score
  , _crEvaluator :: UserResponse
  , _crAgentName :: UserResponse
  , _crWorkflowNumber :: WorkflowNumber
  , _crComment :: Maybe Comment
  , _crClaimType :: ClaimTypeName
  , _crDate :: UTCTime
  } deriving (Show)

$(deriveJSON AO.defaultOptions ''ClaimRecord)
makeClassy ''ClaimRecord

newtype CreateClaimTypes = CreateClaimTypes
  { _cctTypes :: [ClaimTypeName]
  } deriving (Show)

$(deriveJSON AO.defaultOptions ''CreateClaimTypes)
makeClassy ''CreateClaimTypes
