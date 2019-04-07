module Agent.Model.Internal (agentModel) where

import Prelude hiding (get, on, set, (^.))

import Control.Monad.Time (currentTime)
import Database.Esqueleto
  (Entity, InnerJoin(..), entityKey, entityVal, from, in_, on, select, set,
  update, val, valList, where_, (=.), (==.), (^.))
import Database.Persist.Postgresql (fromSqlKey, get, getBy, insert)

import Agent.Model.Types (AgentModel(..))
import Agent.Types (AgentAttrs(..), AgentErrors(..), Bname)
import Common.Types (Id(..))
import Db.Model
import User.Model.Internal (userModel)
import User.Model.Types (HasUserWithId(..), UserModel(..))
import User.Types (Uname(..))

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
             $ maybeToRight SqlErrorFailedToGetAgent
             $ bimap entityVal entityVal <$> safeHead userAgents

   , amUpdateAgent = \userName AgentAttrs{..} -> do
       utcTime <- currentTime
       runExceptT $ do
         mSupervisorId <- getSupervisorId _aaSupervisor
         mBranchId <- getBranchId _aaBranch
         userId <- getUserId userName
         lift $ runInDb $ update $ \agent -> do
                         set agent [ AgentSupervisorId =. val mSupervisorId
                                   , AgentServices =. val _aaServices
                                   , AgentBranch =. val mBranchId
                                   , AgentUpdatedAt =. val utcTime
                                   ]
                         where_ (agent ^. AgentUserId ==. val userId)


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
  }

getUserId :: Uname -> ExceptAgentM m UserId
getUserId name = do
  mUserWithId <- umGetUserByName userModel name
  ExceptT $ pure . maybe (Left $ UserNameNotFound name) (Right . view uiId) $ mUserWithId

getSupervisorId :: Maybe Uname -> ExceptAgentM m (Maybe UserId)
getSupervisorId name = ExceptT $ case name of
   Nothing -> pure . Right $ Nothing
   Just sName -> second Just <$> runExceptT (getUserId sName)

getBranchId :: Maybe Bname -> ExceptAgentM m (Maybe BranchId)
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
