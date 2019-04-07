module Evaluation.Controller
       ( editServiceParameters
       , createParameters
       , saveEvaluation
       , getServiceEvaluations
       ) where
import Data.List (groupBy)
import Database.Persist.Postgresql (fromSqlKey)
import Servant (err400, err401)

import Common.Errors (MonadThrowLogger, eitherSError, throwSError)
import Common.Types (Id(..))
import Evaluation.Model.Types
  (EvalModel(..), EvaluationScore(..), HasEvaluationScore(..),
  HasServiceWithId(..))
import Evaluation.Types
import Model (Evaluation(..), Parameter(..), User(..))
import User.Types (Role(..))

getServiceEvaluations
  :: (MonadThrowLogger m)
  => EvalModel m
  -> Text
  -> m [EvalRecord]
getServiceEvaluations evalModel serviceTyp = do
  service <- emGetService evalModel (ServiceTypeValue serviceTyp) >>= eitherSError err400
  evalScores <- emGetEvaluationByService evalModel (service ^. siId)
  let evalScoresById :: [[EvaluationScore]] =
        groupBy (\es1 es2 -> es1 ^. esEvaluationId == es2 ^. esEvaluationId) evalScores
  pure $ mapMaybe toEvalRecord evalScoresById
  where
    toEvalRecord :: [EvaluationScore] -> Maybe EvalRecord
    toEvalRecord groupScores@(firstScore : _) =
      let _erEvalAttrs = toEvalAttr (ServiceTypeValue serviceTyp) firstScore
          _erParameters = toParameterAttr . view esParameter <$> groupScores
          totalScore =  sum $ view paWeight <$> _erParameters
          isZeroRated = any (\para -> para ^. paCategory == ZeroRated) _erParameters
          _erScore = if isZeroRated then 0 else totalScore
      in Just EvalRecord {..}

    toEvalRecord [] = Nothing

createParameters :: MonadThrowLogger m => EvalModel m -> User -> ServiceParameters -> m [Id]
createParameters evalModel user sp = protectedAction user $ do
   parameterIds <- emCreateParameters evalModel sp >>= eitherSError err400
   pure $ parameterIds <&> Id . fromSqlKey

editServiceParameters :: MonadThrowLogger m => EvalModel m -> User -> ServiceParameters -> m ()
editServiceParameters evalModel user sp =
  protectedAction user $ emEditParameters evalModel sp >>= eitherSError err400


saveEvaluation :: MonadThrowLogger m => EvalModel m -> User -> CreateEvaluation -> m Id
saveEvaluation evalModel user ce = protectedAction user $ do
  evalId <- emCreateEvaluation evalModel ce >>= eitherSError err400
  pure . Id $ fromSqlKey evalId

protectedAction :: MonadThrowLogger m => User -> m a -> m a
protectedAction User{..} action = case userRole of
  Evaluator -> action
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
      evaluator = firstScore ^. esEvaluator
  in EvalAttrs
       { _eaReason = evaluationReason evaluation
       , _eaEvaluator = userName evaluator
       , _eaAgent = userName agent
       , _eaCustomer = evaluationCustomerNumber evaluation
       , _eaService = service
       , _eaDuration = evaluationDuration evaluation
       , _eaComment = evaluationComment evaluation
       , _eaDate = evaluationUpdatedAt evaluation
       }
