module Evaluation.Api
       ( EvaluationApi
       , evaluationServer
       ) where
import Servant
import Servant.Auth.Server

import Common.Types (Id)
import Db.Model (User)
import Evaluation.Controller
  (createParameters, deleteEvaluation, getServiceEvaluations,
  getServiceParamters, saveEvaluation)
import Evaluation.Model.Internal (evalModel)
import Evaluation.Types
  (CreateEvaluation, EvalRecord, ParameterAttrs, ServiceParameters,
  ServiceTypeValue)
import Foundation (App)

type ServiceApi =
         ReqBody '[JSON] ServiceParameters :> Post '[JSON] ()
    :<|> Capture "services" ServiceTypeValue :> Get '[JSON] [ParameterAttrs]

type ProtectedApi =
         Capture "service" ServiceTypeValue :> Get '[JSON] [EvalRecord]
    :<|> ReqBody '[JSON] CreateEvaluation :> Post '[JSON] Id
    :<|> Capture "evaluationId" Id :> Delete '[JSON] ()
    :<|> "services" :> ServiceApi

type EvaluationApi auths = "evaluations" :> Auth auths User :> ProtectedApi

protectedServer
  :: AuthResult User
  -> ServerT ProtectedApi App
protectedServer (Authenticated user) =
         getServiceEvaluations evalModel
    :<|> saveEvaluation evalModel user
    :<|> deleteEvaluation evalModel user
    :<|> createParameters evalModel user
    :<|> getServiceParamters evalModel

protectedServer _ = throwAll err401

evaluationServer :: ServerT (EvaluationApi auths) App
evaluationServer = protectedServer
