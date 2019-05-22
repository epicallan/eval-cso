module Nps.Api
       ( NpsApi
       , npsServer
       ) where
import Servant
import Servant.Auth.Server

import Common.Types (Id)
import Foundation (App)
import Nps.Controller (deleteNps, getNpsRecords, saveNps)
import Nps.Model.Internal (npsModel)
import User.Model.Types (LoggedInUser)
import Nps.Types

type ProtectedApi =
         Get '[JSON] [NpsRecord]
    :<|> ReqBody '[JSON] CreateNps :> Post '[JSON] Id
    :<|> Capture "npsId" Id :> Delete '[JSON] ()

type NpsApi auths = "nps" :> Auth auths LoggedInUser :> ProtectedApi

protectedServer
  :: AuthResult LoggedInUser
  -> ServerT ProtectedApi App
protectedServer (Authenticated user) =
         getNpsRecords npsModel
    :<|> saveNps npsModel user
    :<|> deleteNps npsModel user


protectedServer _ = throwAll err401

npsServer :: ServerT (NpsApi auths) App
npsServer = protectedServer
