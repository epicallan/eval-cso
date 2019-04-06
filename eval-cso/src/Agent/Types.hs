module Agent.Types
       ( AgentErrors (..)
       , AgentResponse(..)
       , AgentAttrs (..)
       , Bname (..)
       , CreateAgent
       , HasAgentAttrs
       , HasCreateAgent (..)
       ) where

import Data.Aeson.Options as AO (defaultOptions)
import Data.Aeson.TH (Options(..), deriveJSON)
import Database.Persist.Sql (PersistField)
import Lens.Micro.Platform (makeClassy)

import Evaluation.Types (ServiceType, ServiceTypeValue)
import User.Types (Uname, UserEdits, UserResponse)

newtype Bname = Name {unBname :: Text}
  deriving (Eq, Show, PersistField)

$(deriveJSON AO.defaultOptions  { unwrapUnaryRecords = True } ''Bname)

data AgentErrors =
    BranchNameNotFound Bname
  | UserNameNotFound Uname
  | SqlErrorFailedToGetAgent
  deriving Show

instance Exception AgentErrors

data AgentAttrs = AgentAttrs
  { _aaSupervisor :: Maybe Uname
  , _aaServices :: Maybe [ServiceTypeValue]
  , _aaBranch :: Maybe Bname
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
  , arBranch :: Maybe Bname
  } deriving Show

$(deriveJSON AO.defaultOptions ''AgentResponse)
