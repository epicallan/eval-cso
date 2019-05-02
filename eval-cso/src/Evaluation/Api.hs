module Evaluation.Api
       ( EvaluationApi
       , evaluationServer
       ) where
import Servant
import Servant.Auth.Server

import Common.Types (Id)
import Db.Model (User)
import Evaluation.Controller
  (createParameters, getServiceEvaluations, getServiceParamters,
  saveEvaluation)
import Evaluation.Model.Internal (evalModel)
import Evaluation.Types
import Foundation (App)

type ServiceApi =
         ReqBody '[JSON] ServiceParameters :> Post '[JSON] ()
    :<|> Capture "services" Text :> Get '[JSON] [ParameterAttrs]

type ProtectedApi =
         Capture "service" Text :> Get '[JSON] [EvalRecord]
    :<|> ReqBody '[JSON] CreateEvaluation :> Post '[JSON] Id
    :<|> "services" :> ServiceApi

type EvaluationApi auths = "evaluations" :> Auth auths User :> ProtectedApi

protectedServer
  :: AuthResult User
  -> ServerT ProtectedApi App
protectedServer (Authenticated user) =
         getServiceEvaluations evalModel
    :<|> saveEvaluation evalModel user
    :<|> createParameters evalModel user
    :<|> getServiceParamters evalModel

protectedServer _ = throwAll err401

evaluationServer :: ServerT (EvaluationApi auths) App
evaluationServer = protectedServer
