module Evaluation.Model.Internal (evalModel) where

import Prelude hiding (on, set, (^.))

import Control.Monad.Time (currentTime)
import Database.Esqueleto hiding ((<&>))

import Common.Types (Id(..))
import Db.Model
import Evaluation.Model.Types
  (EvalModel(..), EvaluationScore(..), ServiceWithId(..))
import Evaluation.Types
  (CreateEvaluation(..), EvalAttrs(..), EvalErrors(..), ParameterAttrs(..),
  Paravalue, ServiceAttrs(..), ServiceParameters(..), ServiceTypeValue)
import qualified User.Types as U (UserName)

evalModel :: forall r m . CanDb m r => EvalModel m
evalModel = EvalModel
  { emCreateParameters = \(ServiceParameters serviceValue parametersAttrs) -> do
      eServiceId <- getServiceId serviceValue
      case eServiceId of
        Left err -> pure $ Left err
        Right serviceId -> do
          parameters <- traverse (mkParameter serviceId) parametersAttrs
          runInDb $ putMany parameters <&> Right

  , emCreateEvaluation = \(CreateEvaluation evalAttrs paramValues) -> do
     eEvaluation <- runExceptT $ mkEvaluation evalAttrs
     case eEvaluation of
       Left errors -> pure $ Left errors
       Right evaluation -> do
         (Entity evalId _) :: Entity Evaluation <- runInDb $ insertEntity evaluation
         mParameterList <- traverse getParameterId paramValues
         scores <- traverse (createParameterScore evalId) (rights mParameterList)
         runInDb $ insertMany scores
         pure $ Right evalId

  , emGetEvaluationByService = \serviceId -> do
     evalData :: [(Entity Evaluation, Entity Parameter, Entity User, Entity User)] <- runInDb $
       select $
         from $ \(service `InnerJoin` evaluation `InnerJoin` parameterScore
                  `InnerJoin` parameter `InnerJoin` agent `InnerJoin` evaluator
                 ) -> do
            on (evaluator ^. UserId ==. evaluation ^. EvaluationEvaluator)
            on (agent ^. UserId ==. evaluation ^. EvaluationAgent)
            on (parameter ^. ParameterId ==. parameterScore ^. ParameterScoreParameter)
            on (parameterScore ^. ParameterScoreEvaluation ==. evaluation ^. EvaluationId)
            on (service ^. ServiceId  ==. evaluation ^. EvaluationServiceType)
            where_ (service ^. ServiceId ==. val serviceId)
            return (evaluation, parameter, agent, evaluator)

     return $ evalData <&> \(evaluation, parameter, agent, evaluator) -> EvaluationScore
                               { _esEvaluation = entityVal evaluation
                               , _esParameter = entityVal parameter
                               , _esAgent = entityVal agent
                               , _esEvaluator = entityVal evaluator
                               , _esEvaluationId = entityKey evaluation
                               }
  , emGetService = \ serviceValue -> do
      eServiceEntity <- getService serviceValue
      pure $ second (\(Entity siId siService) -> ServiceWithId siService siId) eServiceEntity
  , emCreateService = \(ServiceAttrs name value) -> do
      utcTime <- currentTime
      serviceId <- runInDb $ insert
                           $ Service
                              { serviceValue = value
                              , serviceName = name
                              , serviceCreatedAt = utcTime
                              , serviceUpdatedAt = utcTime
                              }
      pure . Id $ fromSqlKey serviceId
  , emGetServiceParameters = \serviceValue -> do
      eServiceEntity <- getService serviceValue
      case eServiceEntity of
        Left err -> pure $ Left err
        Right (Entity serviceId _) -> do
          parameters <- runInDb $
             select $
               from $ \parameter -> do
                 where_ $ parameter ^. ParameterServiceType ==. val serviceId
                 return parameter
          pure . Right $ entityVal <$> parameters
  }
  where
    mkEvaluation :: EvalAttrs -> ExceptT EvalErrors m Evaluation
    mkEvaluation EvalAttrs {..} = do
      utcTime <- currentTime
      agentId  <- ExceptT $ getUserId _eaAgentName
      evaluatorId <- ExceptT $ getUserId _eaEvaluator
      serviceId <- ExceptT $ getServiceId _eaService
      pure Evaluation
        { evaluationEvaluator = evaluatorId
        , evaluationAgent = agentId
        , evaluationServiceType = serviceId
        , evaluationReason = _eaReason
        , evaluationComment = _eaComment
        , evaluationDuration = _eaDuration
        , evaluationCustomerNumber = _eaCustomer
        , evaluationCreatedAt = utcTime
        , evaluationUpdatedAt = utcTime
        }

    getUserId :: U.UserName -> m (Either EvalErrors UserId)
    getUserId uname = do
      meUser :: (Maybe (Entity User)) <- runInDb $ getBy $ UniqueUserName uname
      pure $ maybe (Left $ UserNameNotFound uname ) (Right . entityKey) meUser

    getService :: ServiceTypeValue -> m (Either EvalErrors (Entity Service))
    getService service = do
      meService :: (Maybe (Entity Service)) <- runInDb $ getBy $ UniqueServiceValue service
      pure $ maybeToRight (ServiceNotFound service) meService

    getServiceId :: ServiceTypeValue -> m (Either EvalErrors ServiceId)
    getServiceId service = fmap entityKey <$> getService service

    getParameterId :: Paravalue -> m (Either EvalErrors ParameterId)
    getParameterId pval =  do
      meParameter :: (Maybe (Entity Parameter)) <- runInDb $ getBy $ UniqueParameterValue pval
      pure $ maybe (Left $ ParameterNotFound pval) (Right . entityKey) meParameter

    createParameterScore :: EvaluationId -> ParameterId -> m ParameterScore
    createParameterScore evalId pId = do
      utcTime <- currentTime
      pure ParameterScore
        { parameterScoreEvaluation = evalId
        , parameterScoreParameter = pId
        , parameterScoreCreatedAt = utcTime
        }

    mkParameter :: ServiceId -> ParameterAttrs -> m Parameter
    mkParameter serviceId ParameterAttrs{..} = do
      utcTime <- currentTime
      pure Parameter
        { parameterName = _paName
        , parameterValue = _paValue
        , parameterDescription = _paDescription
        , parameterServiceType = serviceId
        , parameterCategory = _paCategory
        , parameterWeight = _paWeight
        , parameterGroup = _paGroup
        , parameterUpdatedAt = utcTime
        , parameterCreatedAt = utcTime
        }
