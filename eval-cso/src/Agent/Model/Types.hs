module Agent.Model.Types
       ( AgentStorage(..)
       ) where

import Agent.Types (AgentAttrs)
import Common.Types (Id)
import Model (Agent, Branch, BranchId, Service, ServiceId, User, UserId)

data AgentStorage m = AgentStorage
  { asCreateAgent :: UserId -> AgentAttrs -> m Id
  , asGetAgentById :: UserId -> m (Maybe (Agent, User))
  , asUpdateAgent :: UserId -> AgentAttrs-> m ()
  , asAgentBranch :: BranchId -> m (Maybe Branch)
  , asAgentServices :: [ServiceId] -> m [Service]
  }
