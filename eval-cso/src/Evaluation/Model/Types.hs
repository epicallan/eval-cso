module Evaluation.Model.Types
       ( EvalModel(..)
       , EvaluationScore (..)
       , ServiceWithId (..)
       , HasEvaluationScore(..)
       , HasServiceWithId (..)
       ) where
import Lens.Micro.Platform (makeClassy)

import Evaluation.Types
  (CreateEvaluation, EvalErrors, ServiceParameters, ServiceTypeValue)
import Model
  (Evaluation, EvaluationId, Parameter, ParameterId, Service, ServiceId, User)
import User.Types (UserType)

data EvaluationScore = EvaluationScore
  { _esEvaluation :: Evaluation
  , _esParameter :: Parameter
  , _esEvaluator :: User
  , _esAgent :: User
  , _esEvaluationId :: EvaluationId
  }
makeClassy ''EvaluationScore

data ServiceWithId = ServiceWithId
  { _siService :: Service
  , _siId :: ServiceId
  }
makeClassy ''ServiceWithId

data EvalModel m = EvalModel
  { emEditParameters :: ServiceParameters -> m (Either EvalErrors ())
  , emCreateParameters :: ServiceParameters -> m (Either EvalErrors [ParameterId])
  , emCreateEvaluation :: CreateEvaluation -> m (Either EvalErrors EvaluationId)
  , emGetEvaluationByService :: ServiceId -> m [EvaluationScore]
  , emGetService :: ServiceTypeValue -> m (Either EvalErrors ServiceWithId)
  , emGetEvaluationByUser :: forall a . UserType a -> m [EvaluationScore]
  }