module Agent.Model.Internal (agentModel) where

import Prelude hiding (get, on, set, (^.))

import Data.Time (getCurrentTime)
import Database.Esqueleto
  (Entity, InnerJoin(..), entityVal, from, in_, on, select, set, update, val,
  valList, where_, (=.), (==.), (^.))
import Database.Persist.Postgresql (fromSqlKey, get, insert)

import Agent.Model.Types (AgentModel(..))
import Agent.Types (AgentAttrs(..))
import Common.Types (Id(..))
import Foundation (HasPool)
import Model

agentModel :: (MonadIO m, MonadReader r m, HasPool r) => AgentModel m
agentModel = AgentModel
  { amCreateAgent = \userId AgentAttrs{..} -> do
      utcTime <- liftIO getCurrentTime
      agentId <- runInDb $ insert
                   $ Agent
                       { agentUserId = userId
                       , agentSupervisorId = _aaSupervisorId
                       , agentServices = _aaServices
                       , agentBranch = _aaBranch
                       , agentCreatedAt = utcTime
                       , agentUpdatedAt = utcTime
                       }
      pure . Id $ fromSqlKey agentId

   , amGetAgentById = \userId -> do
       userAgents :: [(Entity Agent, Entity User)] <- runInDb $
                       select $
                         from $ \(agent  `InnerJoin` user) -> do
                           on (agent ^. AgentUserId ==. user ^. UserId)
                           where_ (agent ^. AgentUserId ==. val userId)
                           return (agent, user)

       pure $ bimap entityVal entityVal <$> safeHead userAgents

   , amUpdateAgent = \ userId AgentAttrs{..} -> do
       utcTime <- liftIO getCurrentTime
       runInDb $ update $ \agent -> do
                    set agent [ AgentSupervisorId =. val _aaSupervisorId
                              , AgentServices =. val _aaServices
                              , AgentBranch =. val _aaBranch
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

  }
