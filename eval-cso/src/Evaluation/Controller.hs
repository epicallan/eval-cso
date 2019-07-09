module Evaluation.Controller
       ( createParameters
       , saveEvaluation
       , getServiceEvaluations
       , getServiceParamters
       , deleteEvaluation
       ) where
import Data.List (groupBy)
import Database.Persist.Postgresql (fromSqlKey, toSqlKey)
import Servant (err400)

import Common.Errors (MonadThrowLogger, eitherSError)
import Common.Types (Id(..), RecordId(..))
import Db.Model (Evaluation(..), Parameter(..), User(..))
import Evaluation.Model.Types
  (EvalModel(..), EvaluationScore(..), HasEvaluationScore(..),
  HasServiceWithId(..))
import Evaluation.Types
  (Category(ZeroRated), CreateEvaluation, EvalAttrs(..), EvalRecord(..),
  HasParameterAttrs(..), ParameterAttrs(..), ServiceParameters(..),
  ServiceTypeValue(..))
import User.Helper (runAdminAction, runEvaluatorAction)
import User.Model.Types (LoggedInUser)

getServiceEvaluations
  :: (MonadThrowLogger m)
  => EvalModel m
  -> ServiceTypeValue
  -> m [EvalRecord]
getServiceEvaluations evalModel serviceType = do
  service <- emGetService evalModel serviceType >>= eitherSError err400
  evalScores <- emGetEvaluationByService evalModel (service ^. siId)
  let evalScoresById :: [[EvaluationScore]] =
        groupBy (\es1 es2 -> es1 ^. esEvaluationId == es2 ^. esEvaluationId) evalScores
  pure $ mapMaybe toEvalRecord evalScoresById
  where
    toEvalRecord :: [EvaluationScore] -> Maybe EvalRecord
    toEvalRecord groupScores@(firstScore : _) =
      let _erEvalAttrs = toEvalAttr serviceType firstScore
          _erParameters = toParameterAttr . view esParameter <$> groupScores
          totalScore =  sum $ view paWeight <$> _erParameters
          isZeroRated = any (\para -> para ^. paCategory == ZeroRated) _erParameters
          _erScore = if isZeroRated then 0 else 100 - totalScore
          _erId = RecordId . fromSqlKey $ firstScore ^. esEvaluationId
      in Just EvalRecord {..}

    toEvalRecord [] = Nothing


createParameters
  :: MonadThrowLogger m
  => EvalModel m -> LoggedInUser -> ServiceParameters -> m ()
createParameters evalModel user sp = runEvaluatorAction user $
   emCreateParameters evalModel sp >>= eitherSError err400

getServiceParamters
  :: MonadThrowLogger m
  => EvalModel m -> ServiceTypeValue -> m [ParameterAttrs]
getServiceParamters evalModel serviceType = do
   parameters <- emGetServiceParameters evalModel serviceType >>= eitherSError err400
   pure $ toParameterAttr <$> parameters

deleteEvaluation
  :: MonadThrowLogger m
  => EvalModel m -> LoggedInUser -> Id -> m ()
deleteEvaluation evalModel user eId = runAdminAction user $
  emDeleteEvaluation evalModel . toSqlKey $ unId eId -- TODO first check evaluation Exists

saveEvaluation
  :: MonadThrowLogger m
  => EvalModel m -> LoggedInUser -> CreateEvaluation -> m Id
saveEvaluation evalModel user ce = runEvaluatorAction  user $ do
  evalId <- emCreateEvaluation evalModel ce >>= eitherSError err400
  pure . Id $ fromSqlKey evalId

toParameterAttr ::  Parameter -> ParameterAttrs
toParameterAttr parameter =
  let _paCategory = parameterCategory parameter
      _paValue = parameterValue parameter
      _paName = parameterName parameter
      _paDescription = parameterDescription parameter
      _paWeight = parameterWeight parameter
      _paGroup = parameterGroup parameter
  in ParameterAttrs {..}

toEvalAttr
  :: ServiceTypeValue
  -> EvaluationScore
  -> EvalAttrs
toEvalAttr service firstScore =
  let evaluation = firstScore ^. esEvaluation
      agent = firstScore ^. esAgent
      supervisor = firstScore ^. esSupervisor
      evaluator = firstScore ^. esEvaluator
  in EvalAttrs
       { _eaReason = evaluationReason evaluation
       , _eaEvaluator = userName evaluator
       , _eaAgentName = userName agent
       , _eaSupervisor = userFullName <$> supervisor
       , _eaCustomerTel = evaluationCustomerTel evaluation
       , _eaCustomerEmail = evaluationCustomerEmail evaluation
       , _eaService = service
       , _eaDetails = evaluationDetails evaluation
       , _eaAgentBranch =  firstScore ^. esBranch
       , _eaBranch = evaluationBranch evaluation
       , _eaComment = evaluationComment evaluation
       , _eaDate = evaluationUpdatedAt evaluation
       }
