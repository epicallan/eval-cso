module Agent.Model.Types
       ( AgentModel(..)
       , AgentData(..)
       ) where

import Agent.Types (AgentAttrs, AgentErrors, BranchName)
import Common.Types (Id)
import Db.Model (Agent, Branch, BranchId, Service, User, UserId)
import Evaluation.Types (ServiceType, ServiceTypeValue)
import qualified User.Types as U (UserName)

data AgentData = AgentData
  { adServices :: [Service]
  , adBranches :: [Branch]
  , adSupervisors :: [User]
  }

data AgentModel m = AgentModel
  { amCreateAgent :: UserId -> AgentAttrs -> m (Either AgentErrors Id)
  , amGetAgentByName :: U.UserName -> m (Either AgentErrors (Agent, User))
  , amUpdateAgent :: U.UserName -> AgentAttrs-> m (Either AgentErrors ())
  , amAgentBranch :: BranchId -> m (Maybe Branch)
  , amAgentServices :: [ServiceTypeValue] -> m [ServiceType]
  , amGetUserById :: UserId -> m (Maybe User)
  , amCreateBranch :: BranchName -> m Id
  , amGetAgentData :: m AgentData
  }
