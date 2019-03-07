module Agent.Types
       ( AgentErrors (..)
       , AgentResponse(..)
       , AgentAttrs (..)
       , CreateAgent
       , HasAgentAttrs
       , HasCreateAgent (..)
       ) where

import Data.Aeson.Options as AO (defaultOptions)
import Data.Aeson.TH (deriveJSON)
import Lens.Micro.Platform (makeClassy)

import Common.Types (Id, Name)
import Model (BranchId, ServiceId, UserId)
import User.Types (UserEdits, UserResponse)

newtype AgentErrors = AgentUpdateError Id
  deriving Show

instance Exception AgentErrors

data AgentAttrs = AgentAttrs
  { _aaSupervisorId :: Maybe UserId
  , _aaServices :: Maybe [ServiceId]
  , _aaBranch :: Maybe BranchId
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
  , arServices :: Maybe [Name]
  , arBranch :: Maybe Name
  } deriving Show

$(deriveJSON AO.defaultOptions ''AgentResponse)
