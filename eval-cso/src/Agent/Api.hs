module Agent.Api
       ( AgentApi
       , agentServer
       ) where

import Servant
import Servant.Auth.Server

import Agent.Controller
  (createAgentProfile, getAgentByUserName, getAgentData, updateAgent)
import Agent.Model.Internal (agentModel)
import Agent.Types (AgentAttrs, AgentDataResponse, AgentResponse, CreateAgent)
import Common.Types (Id)
import Db.Model (User)
import Foundation (App)
import User.Model.Internal (userModel)

type ProtectedApi =
         Capture "userName" Text :> Get '[JSON] AgentResponse
    :<|> ReqBody '[JSON] CreateAgent :> Post '[JSON] Id
    :<|> Capture "userName" Text :> ReqBody '[JSON] AgentAttrs :> Put '[JSON] ()

type AgentApi auths = "agents" :>
   (    "data" :> Get '[JSON] AgentDataResponse
   :<|> Auth auths User :> ProtectedApi
   )

protectedServer
  :: AuthResult User
  -> ServerT ProtectedApi App
protectedServer (Authenticated user) =
         getAgentByUserName agentModel
    :<|> createAgentProfile agentModel userModel user
    :<|> updateAgent agentModel user

protectedServer _ = throwAll err401

agentServer :: ServerT (AgentApi auths) App
agentServer = getAgentData agentModel :<|> protectedServer
