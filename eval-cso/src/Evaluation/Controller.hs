module Evaluation.Controller
       ( createParameters
       , saveEvaluation
       , getServiceEvaluations
       , getServiceParamters
       ) where
import Data.List (groupBy)
import Database.Persist.Postgresql (fromSqlKey)
import Servant (err400, err401)

import Common.Errors (MonadThrowLogger, eitherSError, throwSError)
import Common.Types (Id(..))
import Db.Model (Evaluation(..), Parameter(..), User(..))
import Evaluation.Model.Types
  (EvalModel(..), EvaluationScore(..), HasEvaluationScore(..),
  HasServiceWithId(..))
import Evaluation.Types
  (Category(ZeroRated), CreateEvaluation, EvalAttrs(..), EvalErrors(..),
  EvalRecord(..), HasParameterAttrs(..), ParameterAttrs(..),
  ServiceParameters(..), ServiceTypeValue(..))
import User.Types (Role(..))

getServiceEvaluations
  :: (MonadThrowLogger m)
  => EvalModel m
  -> Text
  -> m [EvalRecord]
getServiceEvaluations evalModel serviceType = do
  service <- emGetService evalModel (ServiceTypeValue serviceType) >>= eitherSError err400
  evalScores <- emGetEvaluationByService evalModel (service ^. siId)
  let evalScoresById :: [[EvaluationScore]] =
        groupBy (\es1 es2 -> es1 ^. esEvaluationId == es2 ^. esEvaluationId) evalScores
  pure $ mapMaybe toEvalRecord evalScoresById
  where
    toEvalRecord :: [EvaluationScore] -> Maybe EvalRecord
    toEvalRecord groupScores@(firstScore : _) =
      let _erEvalAttrs = toEvalAttr (ServiceTypeValue serviceType) firstScore
          _erParameters = toParameterAttr . view esParameter <$> groupScores
          totalScore =  sum $ view paWeight <$> _erParameters
          isZeroRated = any (\para -> para ^. paCategory == ZeroRated) _erParameters
          _erScore = if isZeroRated then 0 else 100 - totalScore
      in Just EvalRecord {..}

    toEvalRecord [] = Nothing

createParameters
  :: MonadThrowLogger m
  => EvalModel m -> User -> ServiceParameters -> m ()
createParameters evalModel user sp = protectedAction user $
   emCreateParameters evalModel sp >>= eitherSError err400

getServiceParamters
  :: MonadThrowLogger m
  => EvalModel m -> Text -> m [ParameterAttrs]
getServiceParamters evalModel serviceType = do
   let service = ServiceTypeValue serviceType
   parameters <- emGetServiceParameters evalModel service >>= eitherSError err400
   pure $ toParameterAttr <$> parameters

saveEvaluation
  :: MonadThrowLogger m
  => EvalModel m -> User -> CreateEvaluation -> m Id
saveEvaluation evalModel user ce = protectedAction user $ do
  evalId <- emCreateEvaluation evalModel ce >>= eitherSError err400
  pure . Id $ fromSqlKey evalId

protectedAction :: MonadThrowLogger m => User -> m a -> m a
protectedAction User{..} action = case userRole of
  Evaluator -> action
  Admin     -> action
  _         -> throwSError err401 $ ActionIsForEvaluatorsOnly userName

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
       , _eaService = service
       , _eaDetails = evaluationDetails evaluation
       , _eaBranch =  firstScore ^. esBranch
       , _eaComment = evaluationComment evaluation
       , _eaDate = evaluationUpdatedAt evaluation
       }
