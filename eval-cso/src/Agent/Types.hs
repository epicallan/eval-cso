module Agent.Types
       ( AgentErrors (..)
       , AgentResponse(..)
       , AgentDataResponse(..)
       , AgentAttrs (..)
       , BranchName (..)
       , CreateAgent
       , HasAgentAttrs
       , HasCreateAgent (..)
       , HasCreateAgentUser (..)
       ) where

import Data.Aeson.Options as AO (defaultOptions)
import Data.Aeson.TH (deriveJSON)
import Lens.Micro.Platform (makeClassy)

import Evaluation.Types (BranchName(..), ServiceType, ServiceTypeValue)
import User.Types (Email, FullName, UserName, UserResponse)

data AgentErrors =
    BranchNameNotFound BranchName
  | AgentUserNameNotFound UserName
  deriving Show

instance Exception AgentErrors

data AgentAttrs = AgentAttrs
  { _aaSupervisor :: Maybe UserName
  , _aaServices :: Maybe [ServiceTypeValue]
  , _aaBranch :: Maybe BranchName
  } deriving Show

$(deriveJSON AO.defaultOptions ''AgentAttrs)
makeClassy ''AgentAttrs

data CreateAgentUser = CreateAgentUser
 { _cauUserName :: UserName
 , _cauFullName :: FullName
 , _cauEmail :: Email
 } deriving (Show)

makeClassy ''CreateAgentUser
$(deriveJSON AO.defaultOptions ''CreateAgentUser)

data CreateAgent = CreateAgent
  { _caAgent :: AgentAttrs
  , _caUser :: CreateAgentUser
  } deriving Show

$(deriveJSON AO.defaultOptions ''CreateAgent)
makeClassy ''CreateAgent

data AgentResponse = AgentResponse
  { arUser :: UserResponse
  , arSupervisor :: Maybe UserResponse
  , arServices :: Maybe [ServiceType]
  , arBranch :: Maybe BranchName
  } deriving Show

$(deriveJSON AO.defaultOptions ''AgentResponse)

data AgentDataResponse = AgentDataResponse
  { adrServices :: [ServiceType]
  , adrBranches :: [BranchName]
  , adrSupervisors :: [UserResponse]
  }
$(deriveJSON AO.defaultOptions ''AgentDataResponse)
