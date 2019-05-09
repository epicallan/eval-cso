module Nps.Model.Internal (npsModel) where

import Prelude hiding (on, set, (^.))

import Control.Monad.Time (MonadTime, currentTime)
import Database.Esqueleto hiding ((<&>))

import Common.Types (Id(..))
import Db.Model
import Nps.Model.Types (NpsDbRecord(..), NpsModel(..))
import Nps.Types (CreateNps(..), NpsErrors(..))
import User.Model.Internal (userModel)
import User.Model.Types (HasUserWithId(..), UserModel(..))
import qualified User.Types as U (UserName)

type ExceptNpsM m a = forall r. CanDb m r => ExceptT NpsErrors m a

type NpsData =
  ( Entity Nps
  , Entity User
  , Maybe (Entity Branch)
  , Maybe (Entity User)
  , Entity User
  )

npsModel :: forall r m . CanDb m r => NpsModel m
npsModel = NpsModel
  { nmCreateNps = \userId createNps -> do
      eNps <- runExceptT $ mkNps userId createNps
      case eNps of
        Left err -> pure $ Left err
        Right nps -> Right . Id . fromSqlKey <$> runInDb (insert nps)

  , nmGetNpss = do
     npsData :: [NpsData] <- runInDb $
       select $
         from $ \(nps `InnerJoin` agent `InnerJoin` agentProfile `InnerJoin` branch
                  `InnerJoin` supervisor `InnerJoin` evaluator
                 ) -> do
            on (evaluator ^. UserId ==. nps ^. NpsEvaluator)
            on (agentProfile ^. AgentSupervisorId ==. supervisor ?. UserId)
            on (agentProfile ^. AgentBranch ==. branch ?. BranchId)
            on (agent ^. UserId ==. agentProfile ^. AgentUserId)
            on (agent ^. UserId ==. nps ^. NpsAgent)
            return (nps, agent, branch, supervisor, evaluator)
     return $ toNpsDbRecord <$> npsData

  , nmGetEvaluatorId = \uname -> do
      mUserWithId <- umGetUserByName userModel uname
      pure $ maybe (Left $ UserNameNotFound uname) (Right . view uiId) mUserWithId
  }

getUserId :: U.UserName -> ExceptNpsM m UserId
getUserId name = do
  mUserWithId <- umGetUserByName userModel name
  ExceptT $ pure . maybe (Left $ UserNameNotFound name) (Right . view uiId) $ mUserWithId

toNpsDbRecord :: NpsData -> NpsDbRecord
toNpsDbRecord (eNps, eAgent, eBranch, eSupervisor, eEvaluator) = NpsDbRecord
  { ndrNps = entityVal eNps
  , ndrEvaluator = entityVal eEvaluator
  , ndrAgent = entityVal eAgent
  , ndrSupervisor = entityVal <$> eSupervisor
  , ndrBranch = branchName . entityVal <$> eBranch
  }

mkNps :: MonadTime m => UserId -> CreateNps -> ExceptNpsM m Nps
mkNps npsEvaluator CreateNps {..} = do
  npsCreatedAt <- currentTime
  npsAgent <- getUserId cnAgentName
  let npsCustomerTel = cnCustomerTel
      npsDate = cnDate
      npsTouchPoint = cnTouchPoint
      npsRating = cnRating
      npsReason = cnReason
      npsWaitTime = cnWaitTime
      npsDuration = cnDuration
      npsIssueResolved = cnIssueResolved
      npsFurtherInformationGiven = cnFurtherInformationGiven
      npsRatingReason = cnRatingReason
      npsCrmCaptureCorrect = cnCrmCaptureCorrect
      npsFrontLineRatingReason = cnFrontLineRatingReason
      npsCrmCaptureReason = cnCrmCaptureReason
      npsBackOfficeReason = cnBackOfficeReason
      npsUpdatedAt = npsCreatedAt
  pure $ Nps {..}
