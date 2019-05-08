module Nps.Api
       ( NpsApi
       , npsServer
       ) where
import Servant
import Servant.Auth.Server

import Common.Types (Id)
import Db.Model (User)
import Foundation (App)
import Nps.Controller (getNpsRecords, saveNps)
import Nps.Model.Internal (npsModel)
import Nps.Types

type ProtectedApi =
         Get '[JSON] [NpsRecord]
    :<|> ReqBody '[JSON] CreateNps :> Post '[JSON] Id

type NpsApi auths = "nps" :> Auth auths User :> ProtectedApi

protectedServer
  :: AuthResult User
  -> ServerT ProtectedApi App
protectedServer (Authenticated user) =
         getNpsRecords npsModel
    :<|> saveNps npsModel user


protectedServer _ = throwAll err401

npsServer :: ServerT (NpsApi auths) App
npsServer = protectedServer
