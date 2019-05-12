module Nps.Controller
       ( getNpsRecords
       , saveNps
       ) where
import Servant (err400, err401)

import Common.Errors (MonadThrowLogger, eitherSError, throwSError)
import Common.Types (Id(..))
import Db.Model (Nps(..), User(..))
import Nps.Model.Types (NpsDbRecord(..), NpsModel(..))
import Nps.Types (CreateNps, NpsErrors(..), NpsRecord(..))
import User.Types (Role(..))

getNpsRecords
  :: MonadThrowLogger m
  => NpsModel m
  -> m [NpsRecord]
getNpsRecords npsModel = do
  npsDbRecords <- nmGetNpss npsModel
  pure $ toNpsRecord <$> npsDbRecords

saveNps
  :: MonadThrowLogger m
  => NpsModel m -> User -> CreateNps -> m Id
saveNps npsModel user nps = protectedAction user $ do
  evaluatorId <- nmGetEvaluatorId npsModel (userName user) >>= eitherSError err401
  nmCreateNps npsModel evaluatorId nps >>= eitherSError err400

protectedAction :: MonadThrowLogger m => User -> m a -> m a
protectedAction User{..} action = case userRole of
  Evaluator -> action
  Admin     -> action
  _         -> throwSError err401 $ ActionIsForEvaluators userName

toNpsRecord :: NpsDbRecord -> NpsRecord
toNpsRecord NpsDbRecord{..} =
  let nrEvaluator = userName ndrEvaluator
      nrAgentName = userName ndrAgent
      nrSupervisor = userFullName <$> ndrSupervisor
      nrBranch = ndrBranch
      nrCustomerTel = npsCustomerTel ndrNps
      nrDate = npsDate ndrNps
      nrTouchPoint = npsTouchPoint ndrNps
      nrRating = npsRating ndrNps
      nrReason = npsReason ndrNps
      nrWaitTime = npsWaitTime ndrNps
      nrDuration = npsDuration ndrNps
      nrIssueResolved = npsIssueResolved ndrNps
      nrFurtherInformationGinven = npsFurtherInformationGiven ndrNps
      nrRatingReason = npsRatingReason ndrNps
      nrCrmCaptureCorrect = npsCrmCaptureCorrect ndrNps
      nrCrmCaptureReason = npsCrmCaptureReason ndrNps
      nrFrontLineRatingReasons = npsFrontLineRatingReasons ndrNps
      nrBackOfficeReasons = npsBackOfficeReasons ndrNps
      nrCreatedAt = npsCreatedAt ndrNps
  in NpsRecord{..}
