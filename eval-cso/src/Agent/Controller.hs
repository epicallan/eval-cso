module Agent.Controller
       ( createAgentProfile
       , updateAgent
       , getAgentById
       ) where

import Control.Monad.Time (MonadTime)
import Database.Persist.Postgresql (toSqlKey)
import Servant (err400)

import Agent.Model.Types (AgentStorage(..))
import Agent.Types
  (AgentAttrs, AgentErrors(..), AgentResponse(..), HasCreateAgent(..))
import Common.Errors (throwSError)
import Common.Types (Id(..))
import Foundation (HasConfig)
import Model (Agent(..), Branch(..), Service(..), User)
import User.Controller (generateUser)
import User.Helper (runProtectedAction, toUserResponse)
import User.Model.Types (UserModel(..))
import User.Types (Role(Member))

-- | called when an agent is being auto generated by admin or evaluator
createAgentProfile
  :: (HasConfig r, MonadReader r m, MonadTime m, MonadThrow m
     , HasCreateAgent attrs
     )
  => AgentStorage m
  -> UserModel m
  -> User -- ^ logged in user
  -> attrs -- ^ A combination of user and agent specific attributes
  -> m Id
createAgentProfile astorage us logedInUser attrs = do
  uid <- generateUser us logedInUser $ attrs ^. caUserAttrs
  asCreateAgent astorage (toSqlKey $ unId uid) (attrs ^. caAgentAttrs)

-- | used in creating agent profile when agent exists after signup as a user
updateAgent
  :: MonadThrow m
  => AgentStorage m
  -> UserModel m
  -> User -- ^ The current logged in user
  -> Int64 -- ^ Agent User Id
  -> AgentAttrs
  -> m AgentResponse
updateAgent astorage us logedInUser uid attrs = do
  let userId = toSqlKey uid

  runProtectedAction
    logedInUser
    Member $
    asUpdateAgent astorage userId attrs

  getAgentById astorage us uid

getAgentById
  :: MonadThrow m
  => AgentStorage m
  -> UserModel m
  -> Int64 -- ^ Agent user Id
  -> m AgentResponse
getAgentById astorage us uid = do
  mAgentUser <- asGetAgentById astorage (toSqlKey uid)
  let toAgentResponse' = uncurry $ toAgentResponse astorage us
      throwUpdateError = throwSError err400 $ AgentUpdateError $ Id uid
  maybe throwUpdateError toAgentResponse' mAgentUser

toAgentResponse
  :: MonadThrow m
  => AgentStorage m
  -> UserModel m
  -> Agent
  -> User -- ^ Agent User attributes
  -> m AgentResponse
toAgentResponse astorage us agent user = do
  mSupervisor <- traverse (usGetUserById us) $ agentSupervisorId agent
  mBranch <- traverse (asAgentBranch astorage) $ agentBranch agent
  mServices <- traverse (asAgentServices astorage) $ agentServices agent
  let arSupervisor = toUserResponse <$> join mSupervisor
      arBranch = branchName <$> join mBranch
      arServices = fmap serviceName <$> mServices
      arUser = toUserResponse user
  pure AgentResponse {..}
