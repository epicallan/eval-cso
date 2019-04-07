module Agent.Api
       ( AgentApi
       , agentServer
       ) where

import Servant
import Servant.Auth.Server

import Agent.Controller (createAgentProfile, getAgentByUserName, updateAgent)
import Agent.Model.Internal (agentModel)
import Agent.Types (AgentAttrs, AgentResponse, CreateAgent)
import Common.Types (Id)
import Foundation (App)
import Db.Model (User)
import User.Model.Internal (userModel)

type ProtectedApi =
         Capture "userName" Text :> Get '[JSON] AgentResponse
    :<|> ReqBody '[JSON] CreateAgent :> Post '[JSON] Id
    :<|> Capture "userName" Text :> ReqBody '[JSON] AgentAttrs :> Put '[JSON] ()

type AgentApi auths = "agents" :> Auth auths User :> ProtectedApi

protectedServer
  :: AuthResult User
  -> ServerT ProtectedApi App
protectedServer (Authenticated user) =
         getAgentByUserName agentModel
    :<|> createAgentProfile agentModel userModel user
    :<|> updateAgent agentModel user

protectedServer _ = throwAll err401

agentServer :: ServerT (AgentApi auths) App
agentServer = protectedServer
