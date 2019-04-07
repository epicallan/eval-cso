module Agent.Model.Types
       ( AgentModel(..)
       ) where

import Agent.Types (AgentAttrs, AgentErrors, Bname)
import Common.Types (Id)
import Db.Model (Agent, Branch, BranchId, User, UserId)
import Evaluation.Types (ServiceType, ServiceTypeValue)
import User.Types (Uname)

data AgentModel m = AgentModel
  { amCreateAgent :: UserId -> AgentAttrs -> m (Either AgentErrors Id)
  , amGetAgentByName :: Uname -> m (Either AgentErrors (Agent, User))
  , amUpdateAgent :: Uname -> AgentAttrs-> m (Either AgentErrors ())
  , amAgentBranch :: BranchId -> m (Maybe Branch)
  , amAgentServices :: [ServiceTypeValue] -> m [ServiceType]
  , amGetUserById :: UserId -> m (Maybe User)
  , amCreateBranch :: Bname -> m Id
  }
