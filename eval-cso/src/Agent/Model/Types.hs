module Agent.Model.Types
       ( AgentModel(..)
       ) where

import Agent.Types (AgentAttrs)
import Common.Types (Id)
import Model (Agent, Branch, BranchId, Service, ServiceId, User, UserId)

data AgentModel m = AgentModel
  { amCreateAgent :: UserId -> AgentAttrs -> m Id
  , amGetAgentById :: UserId -> m (Maybe (Agent, User))
  , amUpdateAgent :: UserId -> AgentAttrs-> m ()
  , amAgentBranch :: BranchId -> m (Maybe Branch)
  , amAgentServices :: [ServiceId] -> m [Service]
  }
