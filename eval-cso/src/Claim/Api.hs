module Claim.Api
       ( ClaimApi
       , claimServer
       ) where
import Servant
import Servant.Auth.Server

import Claim.Controller (getClaimTypes, getClaims, saveClaim, saveClaimTypes)
import Claim.Model.Internal (claimModel)
import Claim.Types (ClaimRecord, ClaimTypeRecord, CreateClaim)
import Common.Types (Id)
import Db.Model (User)
import Foundation (App)

type ClaimTypeApi =
         ReqBody '[JSON] [ClaimTypeRecord] :> Post '[JSON] ()
    :<|> Get '[JSON] [ClaimTypeRecord]

type ProtectedApi =
         Get '[JSON] [ClaimRecord]
    :<|> ReqBody '[JSON] CreateClaim :> Post '[JSON] Id
    :<|> "types" :> ClaimTypeApi

type ClaimApi auths = "claims" :> Auth auths User :> ProtectedApi

protectedServer
  :: AuthResult User
  -> ServerT ProtectedApi App
protectedServer (Authenticated user) =
         getClaims claimModel
    :<|> saveClaim claimModel user
    :<|> saveClaimTypes claimModel user
    :<|> getClaimTypes claimModel

protectedServer _ = throwAll err401

claimServer :: ServerT (ClaimApi auths) App
claimServer = protectedServer
