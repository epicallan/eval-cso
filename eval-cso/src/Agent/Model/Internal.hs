module Agent.Model.Internal (agentModel) where

import Prelude hiding (get, on, set, (^.))

import Control.Monad.Time (currentTime)
import Database.Esqueleto
import qualified Database.Persist.Postgresql as P (upsert, (=.))

import Agent.Model.Types (AgentData(..), AgentModel(..))
import Agent.Types (AgentAttrs(..), AgentErrors(..))
import qualified Agent.Types as A (BranchName)
import Common.Types (Id(..))
import Db.Model
import User.Model.Internal (userModel)
import User.Model.Types (HasUserWithId(..), UserModel(..))
import User.Types (Role(Supervisor))
import qualified User.Types as U (UserName(..))

type ExceptAgentM m a = forall r. CanDb m r => ExceptT AgentErrors m a

agentModel :: forall r m . CanDb m r => AgentModel m
agentModel = AgentModel
  { amCreateAgent = runExceptT ... createAgent

   , amGetAgentByName = \name -> do
       eUserId <- runExceptT $ getUserId name
       case eUserId of
         Left err -> pure $ Left err
         Right userId -> do
           userAgents :: [(Entity Agent, Entity User)] <- runInDb $
                           select $
                             from $ \(agent  `InnerJoin` user) -> do
                               on (agent ^. AgentUserId ==. user ^. UserId)
                               where_ (agent ^. AgentUserId ==. val userId)
                               return (agent, user)

           pure
             $ maybeToRight (AgentUserNameNotFound name)
             $ bimap entityVal entityVal <$> safeHead userAgents

   , amUpdateAgent = \userName AgentAttrs{..} -> do
       utcTime <- currentTime
       runExceptT $ do
         mSupervisorId <- getSupervisorId _aaSupervisor
         mBranchId <- getBranchId _aaBranch
         userId <- getUserId userName
         let newAgent = Agent
                         { agentUserId = userId
                         , agentSupervisorId = mSupervisorId
                         , agentServices = Nothing
                         , agentBranch = mBranchId
                         , agentCreatedAt = utcTime
                         , agentUpdatedAt = utcTime
                         }
         let agentUpdates = [ AgentSupervisorId P.=. mSupervisorId
                            , AgentServices P.=.  _aaServices
                            , AgentBranch P.=. mBranchId
                            , AgentUpdatedAt P.=. utcTime
                            ]
         void $ lift $ runInDb $ P.upsert newAgent agentUpdates

   , amAgentServices = \serviceTypeValues -> do
       services :: [Entity Service] <- runInDb $
                     select $
                       from $ \service -> do
                        where_ $ service ^. ServiceValue `in_` valList serviceTypeValues
                        return service

       traverse (pure . serviceName . entityVal)  services

    , amAgentBranch = runInDb . get
    , amGetUserById = umGetUserById userModel
    , amCreateBranch = \bname -> do
        utcTime <- currentTime
        branchId <- runInDb $ insert
                            $ Branch
                               { branchName = bname
                               , branchCreatedAt = utcTime
                               , branchUpdatedAt = utcTime
                                }
        pure . Id . fromSqlKey $ branchId
    , amGetAgentData = do
        eBranches :: [Entity Branch] <- runInDb $
          select $ from $ \branch -> return branch
        eServices :: [Entity Service] <- runInDb $
          select $ from $ \service -> return service
        eSupervisors :: [Entity User] <- runInDb $
           select $
             from $ \user -> do
               where_ $ user ^. UserRole ==. val Supervisor
               return user
        let branches = fmap entityVal eBranches
            services = fmap entityVal eServices
            supervisors = fmap entityVal eSupervisors
        return AgentData
          { adServices = services
          , adBranches = branches
          , adSupervisors = supervisors
          }
  }

getUserId :: U.UserName -> ExceptAgentM m UserId
getUserId name = do
  mUserWithId <- umGetUserByName userModel name
  ExceptT $ pure . maybe (Left $ AgentUserNameNotFound name) (Right . view uiId) $ mUserWithId

getSupervisorId :: Maybe U.UserName -> ExceptAgentM m (Maybe UserId)
getSupervisorId name = ExceptT $ case name of
   Nothing -> pure . Right $ Nothing
   Just sName -> second Just <$> runExceptT (getUserId sName)

getBranchId :: Maybe A.BranchName -> ExceptAgentM m (Maybe BranchId)
getBranchId name = case name of
  Nothing -> ExceptT . pure . Right $ Nothing
  Just bName -> do
    mBranch <- lift $ runInDb $ getBy $ UniqueBranchName bName
    ExceptT . pure
      $ maybe (Left $ BranchNameNotFound bName) (Right . Just . entityKey) mBranch

createAgent :: UserId -> AgentAttrs -> ExceptAgentM m Id
createAgent userId AgentAttrs{..}= do
  utcTime <- currentTime
  mSupervisorId <- getSupervisorId _aaSupervisor
  mBranchId <- getBranchId _aaBranch
  agentId <- lift $ runInDb
                  $ insert
                  $ Agent
                       { agentUserId = userId
                       , agentSupervisorId = mSupervisorId
                       , agentServices = _aaServices
                       , agentBranch = mBranchId
                       , agentCreatedAt = utcTime
                       , agentUpdatedAt = utcTime
                       }
  pure . Id . fromSqlKey $ agentId
