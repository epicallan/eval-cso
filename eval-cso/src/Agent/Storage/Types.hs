module Agent.Storage.Types (AgentStorage) where

import Common.Types (Id)
import Model (Agent, UserId)
import Agent.Types (AgentErrors)

data AgentStorage m = AgentStorage
  { asCreateAgent :: Agent -> m Id
  , asAllAgents :: m [Agent]
  , asGetAgentById :: UserId -> m (Either AgentErrors Id)
  , asUpdateAgent :: UserId -> Edits -> m User
  }
