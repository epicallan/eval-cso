module Nps.Controller
       ( getNpsRecords
       , saveNps
       , deleteNps
       ) where
import Database.Persist.Postgresql (fromSqlKey, toSqlKey)
import Servant (err400, err401)

import Common.Errors (MonadThrowLogger, eitherSError)
import Common.Types (Id, RecordId(..))
import Db.Model (Nps(..), User(..))
import Nps.Model.Types (NpsDbRecord(..), NpsModel(..))
import Nps.Types (CreateNps, NpsRecord(..))
import User.Helper (runAdminAction, runEvaluatorAction)

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
saveNps npsModel user nps = runEvaluatorAction user $ do
  evaluatorId <- nmGetEvaluatorId npsModel (userName user) >>= eitherSError err401
  nmCreateNps npsModel evaluatorId nps >>= eitherSError err400

deleteNps
  :: MonadThrowLogger m
  => NpsModel m -> User -> Int64 -> m ()
deleteNps npsModel user npsId = runAdminAction user $
  nmDeleteNps npsModel (toSqlKey npsId)

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
      nrId = RecordId $ fromSqlKey ndrNpsId
      nrCreatedAt = npsCreatedAt ndrNps
  in NpsRecord{..}
