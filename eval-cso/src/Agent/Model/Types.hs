module Agent.Model.Types
       ( AgentModel(..)
       ) where

import Agent.Types (AgentAttrs, AgentErrors)
import User.Types (Uname)
import Common.Types (Id)
import Evaluation.Types (ServiceTypeValue, ServiceType)
import Model (Agent, Branch, BranchId, User, UserId)

data AgentModel m = AgentModel
  { amCreateAgent :: UserId -> AgentAttrs -> m (Either AgentErrors Id)
  , amGetAgentByName :: Uname -> m (Either AgentErrors (Agent, User))
  , amUpdateAgent :: Uname -> AgentAttrs-> m (Either AgentErrors ())
  , amAgentBranch :: BranchId -> m (Maybe Branch)
  , amAgentServices :: [ServiceTypeValue] -> m [ServiceType]
  , amGetUserById :: UserId -> m (Maybe User)
  }
