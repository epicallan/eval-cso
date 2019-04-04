module Evaluation.Api
       ( EvaluationApi
       , evaluationServer
       ) where
import Servant
import Servant.Auth.Server

import Common.Types (Id)
import Evaluation.Controller
  (createParameters, editServiceParameters, getServiceEvaluations,
  saveEvaluation)
import Evaluation.Model.Internal (evalModel)
import Evaluation.Types
import Foundation (App)
import Model (User)

type ProtectedApi =
         Capture "serviceType" Text :> Get '[JSON] [EvalRecord]
    :<|> ReqBody '[JSON] ServiceParameters :> Post '[JSON] [Id]
    :<|> ReqBody '[JSON] ServiceParameters :> Put '[JSON] ()
    :<|> ReqBody '[JSON] CreateEvaluation :> Post '[JSON] Id

type EvaluationApi auths = "evaluation" :> Auth auths User :> ProtectedApi

protectedServer
  :: AuthResult User
  -> ServerT ProtectedApi App
protectedServer (Authenticated user) =
         getServiceEvaluations evalModel
    :<|> createParameters evalModel user
    :<|> editServiceParameters evalModel user
    :<|> saveEvaluation evalModel user

protectedServer _ = throwAll err401

evaluationServer :: ServerT (EvaluationApi auths) App
evaluationServer = protectedServer

