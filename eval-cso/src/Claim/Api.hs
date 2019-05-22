module Claim.Api
       ( ClaimApi
       , claimServer
       ) where
import Servant
import Servant.Auth.Server

import Claim.Controller
  (deleteClaim, getClaimTypes, getClaims, saveClaim, saveClaimTypes)
import Claim.Model.Internal (claimModel)
import Claim.Types (ClaimRecord, ClaimTypeRecord, CreateClaim)
import Common.Types (Id)
import Foundation (App)
import User.Model.Types (LoggedInUser)

type ClaimTypeApi =
         ReqBody '[JSON] [ClaimTypeRecord] :> Post '[JSON] ()
    :<|> Get '[JSON] [ClaimTypeRecord]

type ProtectedApi =
         Get '[JSON] [ClaimRecord]
    :<|> ReqBody '[JSON] CreateClaim :> Post '[JSON] Id
    :<|> Capture "claimId" Id :> Delete '[JSON] ()
    :<|> "types" :> ClaimTypeApi

type ClaimApi auths = "claims" :> Auth auths LoggedInUser :> ProtectedApi

protectedServer
  :: AuthResult LoggedInUser
  -> ServerT ProtectedApi App
protectedServer (Authenticated user) =
         getClaims claimModel
    :<|> saveClaim claimModel user
    :<|> deleteClaim claimModel user
    :<|> saveClaimTypes claimModel user
    :<|> getClaimTypes claimModel


protectedServer _ = throwAll err401

claimServer :: ServerT (ClaimApi auths) App
claimServer = protectedServer
