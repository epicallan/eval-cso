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

npsModel :: forall r m . CanDb m r => NpsModel m
npsModel = NpsModel
  { nmCreateNps = \userId createNps -> do
      eNps <- runExceptT $ mkNps userId createNps
      case eNps of
        Left err -> pure $ Left err
        Right nps -> Right . Id . fromSqlKey <$> runInDb (insert nps)

  , nmGetNpss = do
     npsData :: [(Entity Nps, Entity User, Entity User)] <- runInDb $
       select $
         from $ \(nps `InnerJoin` agent `InnerJoin` evaluator) -> do
            on (evaluator ^. UserId ==. nps ^. NpsEvaluator)
            on (agent ^. UserId ==. nps ^. NpsAgent)
            return (nps, agent, evaluator)
     return $ toNpsDbRecord <$> npsData

  , nmGetEvaluatorId = \uname -> do
      mUserWithId <- umGetUserByName userModel uname
      pure $ maybe (Left $ UserNameNotFound uname) (Right . view uiId) mUserWithId
  }

getUserId :: U.UserName -> ExceptNpsM m UserId
getUserId name = do
  mUserWithId <- umGetUserByName userModel name
  ExceptT $ pure . maybe (Left $ UserNameNotFound name) (Right . view uiId) $ mUserWithId

toNpsDbRecord :: (Entity Nps, Entity User, Entity User) -> NpsDbRecord
toNpsDbRecord (eNps, eAgent, eEvaluator) = NpsDbRecord
  { ndrNps = entityVal eNps
  , ndrEvaluator = entityVal eEvaluator
  , ndrAgent = entityVal eAgent
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
