module Agent.Api
       ( AgentApi
       , agentServer
       ) where

import Servant
import Servant.Auth.Server

import Agent.Controller (createAgentProfile, getAgentById, updateAgent)
import Agent.Model.Internal (agentModel)
import Agent.Types (AgentAttrs, AgentResponse, CreateAgent)
import Common.Types (Id)
import Foundation (App)
import Model (User)
import User.Model.Internal (userModel)

type ProtectedApi =
         Capture "id" Int64 :> Get '[JSON] AgentResponse
    :<|> ReqBody '[JSON] CreateAgent :> Post '[JSON] Id
    :<|> Capture "id" Int64 :> ReqBody '[JSON] AgentAttrs :> Put '[JSON] AgentResponse

type AgentApi auths = "agent" :> Auth auths User :> ProtectedApi

protectedServer
  :: AuthResult User
  -> ServerT ProtectedApi App
protectedServer (Authenticated user) =
         getAgentById agentModel userModel
    :<|> createAgentProfile agentModel userModel user
    :<|> updateAgent agentModel userModel user

protectedServer _ = throwAll err401

agentServer :: ServerT (AgentApi auths) App
agentServer = protectedServer
