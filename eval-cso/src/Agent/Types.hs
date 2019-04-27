module Agent.Types
       ( AgentErrors (..)
       , AgentResponse(..)
       , AgentAttrs (..)
       , BranchName (..)
       , CreateAgent
       , HasAgentAttrs
       , HasCreateAgent (..)
       ) where

import Data.Aeson.Options as AO (defaultOptions)
import Data.Aeson.TH (Options(..), deriveJSON)
import Database.Persist.Sql (PersistField)
import Lens.Micro.Platform (makeClassy)

import Evaluation.Types (ServiceType, ServiceTypeValue)
import User.Types (UserName, UserEdits, UserResponse)

newtype BranchName = Name {unBname :: Text}
  deriving (Eq, Show, PersistField)

$(deriveJSON AO.defaultOptions  { unwrapUnaryRecords = True } ''BranchName)

data AgentErrors =
    BranchNameNotFound BranchName
  | UserNameNotFound UserName
  | SqlErrorFailedToGetAgent
  deriving Show

instance Exception AgentErrors

data AgentAttrs = AgentAttrs
  { _aaSupervisor :: Maybe UserName
  , _aaServices :: Maybe [ServiceTypeValue]
  , _aaBranch :: Maybe BranchName
  } deriving Show

$(deriveJSON AO.defaultOptions ''AgentAttrs)
makeClassy ''AgentAttrs

data CreateAgent = CreateAgent
  { _caAgentAttrs :: AgentAttrs
  , _caUserAttrs :: UserEdits
  }

$(deriveJSON AO.defaultOptions ''CreateAgent)
makeClassy ''CreateAgent

data AgentResponse = AgentResponse
  { arUser :: UserResponse
  , arSupervisor :: Maybe UserResponse
  , arServices :: Maybe [ServiceType]
  , arBranch :: Maybe BranchName
  } deriving Show

$(deriveJSON AO.defaultOptions ''AgentResponse)
