module Evaluation.Model.Types
       ( EvalModel(..)
       , EvaluationScore (..)
       , ServiceWithId (..)
       , HasEvaluationScore(..)
       , HasServiceWithId (..)
       ) where
import Lens.Micro.Platform (makeClassy)

import Common.Types (Id)
import Db.Model (Evaluation, EvaluationId, Parameter, Service, ServiceId, User)
import Evaluation.Types
  (BranchName, CreateEvaluation, EvalErrors, ServiceAttrs, ServiceParameters,
  ServiceTypeValue)

data EvaluationScore = EvaluationScore
  { _esEvaluation :: Evaluation
  , _esParameter :: Maybe Parameter
  , _esEvaluator :: User
  , _esAgent :: User
  , _esSupervisor :: Maybe User
  , _esBranch :: Maybe BranchName
  , _esEvaluationId :: EvaluationId
  }
makeClassy ''EvaluationScore

data ServiceWithId = ServiceWithId
  { _siService :: Service
  , _siId :: ServiceId
  }
makeClassy ''ServiceWithId

data EvalModel m = EvalModel
  { emCreateParameters :: ServiceParameters -> m (Either EvalErrors ())
  , emCreateEvaluation :: CreateEvaluation -> m (Either EvalErrors EvaluationId)
  , emGetEvaluationByService :: ServiceId -> m [EvaluationScore]
  , emGetService :: ServiceTypeValue -> m (Either EvalErrors ServiceWithId)
  , emCreateService :: ServiceAttrs -> m Id
  , emGetServiceParameters :: ServiceTypeValue -> m (Either EvalErrors [Parameter])
  , emDeleteEvaluation :: EvaluationId ->  m ()
  }
