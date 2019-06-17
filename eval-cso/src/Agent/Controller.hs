module Agent.Controller
       ( createAgentProfile
       , createAgentProfile_
       , updateAgent
       , getAgentByUserName
       , getAgentData
       ) where

import Control.Monad.Time (MonadTime)
import Database.Persist.Postgresql (toSqlKey)
import Servant (err400)

import Agent.Model.Types (AgentData(..), AgentModel(..))
import Agent.Types
  (AgentAttrs, AgentDataResponse(..), AgentResponse(..), CreateAgent,
  HasCreateAgent(..), HasCreateAgentUser(..))
import Common.Errors (MonadThrowLogger, eitherSError)
import Common.Types (Id(..))
import Db.Model (Agent(..), Branch(..), Service(..), User)
import Foundation (HasSettings)
import User.Controller (generateUser, generateUser_)
import User.Helper (runProtectedAction, toUserResponse)
import User.Model.Types (LoggedInUser, UserModel(..))
import User.Types (Role(CSOAgent), UserEdits(..))
import qualified User.Types as U (UserName(..))

-- | called when an agent is being auto generated by admin or evaluator
createAgentProfile
  :: (HasSettings r, MonadReader r m, MonadTime m, MonadThrowLogger m, MonadIO m
     )
  => AgentModel m
  -> UserModel m
  -> LoggedInUser
  -> CreateAgent
  -> m Id
createAgentProfile agentModel usModel logedInUser attrs = do
  uid <- generateUser usModel logedInUser agentUser
  eitherSError err400 =<<
    amCreateAgent agentModel (toSqlKey $ unId uid) (attrs ^. caAgent)
  where
    agentUser :: UserEdits
    agentUser = UserEdits
      { _userEditsEmail = attrs ^. (caUser . cauEmail)
      , _userEditsFullName = attrs ^. (caUser . cauFullName)
      , _userEditsRole = CSOAgent
      , _userEditsUserName = attrs ^. (caUser . cauUserName)
      }

-- | used for db migration
createAgentProfile_
  :: (HasSettings r, MonadReader r m, MonadTime m, MonadThrowLogger m, MonadIO m
     )
  => AgentModel m
  -> UserModel m
  -> CreateAgent
  -> m Id
createAgentProfile_ agentModel usModel attrs = do
  uid <- generateUser_ usModel agentUser
  either throwM pure  =<<
    amCreateAgent agentModel (toSqlKey $ unId uid) (attrs ^. caAgent)
  where
    agentUser :: UserEdits
    agentUser = UserEdits
      { _userEditsEmail = attrs ^. (caUser . cauEmail)
      , _userEditsFullName = attrs ^. (caUser . cauFullName)
      , _userEditsRole = CSOAgent
      , _userEditsUserName = attrs ^. (caUser . cauUserName)
      }

-- | used in creating agent profile when agent exists after signup as a user
updateAgent
  :: MonadThrowLogger m
  => AgentModel m
  -> LoggedInUser
  -> U.UserName
  -> AgentAttrs
  -> m ()
updateAgent agentModel logedInUser userName attrs =
  eitherSError err400 =<< runProtectedAction
    logedInUser
    CSOAgent
    userName
    (amUpdateAgent agentModel userName attrs)

getAgentByUserName
  :: MonadThrowLogger m
  => AgentModel m
  -> U.UserName
  -> m AgentResponse
getAgentByUserName agentModel userName = do
  agentUser <- eitherSError err400 =<< amGetAgentByName agentModel userName
  uncurry (toAgentResponse agentModel) agentUser

getAgentData :: Functor m => AgentModel m -> m AgentDataResponse
getAgentData = fmap toAgentDataResponse . amGetAgentData

toAgentResponse
  :: MonadThrowLogger m
  => AgentModel m
  -> Agent
  -> User -- ^ Agent User attributes
  -> m AgentResponse
toAgentResponse agentModel agent user = do
  mSupervisor <- traverse (amGetUserById agentModel) $ agentSupervisorId agent
  mBranch <- traverse (amAgentBranch agentModel) $ agentBranch agent
  arServices <- traverse (amAgentServices agentModel) $ agentServices agent
  let arSupervisor = toUserResponse <$> join mSupervisor
      arBranch = branchName <$> join mBranch
      arUser = toUserResponse user
  pure AgentResponse {..}

toAgentDataResponse :: AgentData -> AgentDataResponse
toAgentDataResponse AgentData{..} =
  let adrBranches = branchName <$> adBranches
      adrServices = serviceName <$> adServices
      adrSupervisors = toUserResponse <$> adSupervisors
  in AgentDataResponse {..}
