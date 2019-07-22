{-# LANGUAGE ImplicitParams #-}
module Nps.Controller
       ( getNpsRecords
       , saveNps
       , deleteNps
       ) where
import Database.Persist.Postgresql (fromSqlKey, toSqlKey)
import Servant (err400, err401)

import Common.Errors (MonadThrowLogger, eitherSError)
import Common.Types (Id(..), RecordId(..))
import Db.Model (Nps(..), User(..))
import Nps.Model.Types (NpsDbRecord(..), NpsModel(..))
import Nps.Types (CreateNps, NpsRecord(..))
import User.Helper (runAdminAction, runEvaluatorAction)
import User.Model.Types (LoggedInUser, SafeUser(..))

type NpsEff m = (MonadThrowLogger m, ?npsModel :: NpsModel m)

getNpsRecords
  :: NpsEff m
  => m [NpsRecord]
getNpsRecords = do
  npsDbRecords <- nmGetNpss ?npsModel
  pure $ toNpsRecord <$> npsDbRecords

saveNps
  :: NpsEff m
  => LoggedInUser -> CreateNps -> m Id
saveNps loggedInUser nps = runEvaluatorAction loggedInUser $ do
  evaluatorId <- nmGetEvaluatorId ?npsModel (userName user) >>= eitherSError err401
  nmCreateNps ?npsModel evaluatorId nps >>= eitherSError err400
  where
    user = unSafeUser loggedInUser

deleteNps
  :: NpsEff m
  => LoggedInUser -> Id -> m ()
deleteNps user npsId = runAdminAction user $
  nmDeleteNps ?npsModel . toSqlKey $ unId npsId

toNpsRecord :: NpsDbRecord -> NpsRecord
toNpsRecord NpsDbRecord{..} =
  let nrEvaluator = userName ndrEvaluator
      nrAgentName = userName ndrAgent
      nrSupervisor = userFullName <$> ndrSupervisor
      nrCustomerTel = npsCustomerTel ndrNps
      nrDate = npsDate ndrNps
      nrBranch = npsBranch ndrNps
      nrRating = npsRating ndrNps
      nrReason = npsReason ndrNps
      nrWaitTime = npsWaitTime ndrNps
      nrDuration = npsDuration ndrNps
      nrIssueResolved = npsIssueResolved ndrNps
      nrFurtherInformationGiven = npsFurtherInformationGiven ndrNps
      nrRatingReason = npsRatingReason ndrNps
      nrCrmCaptureCorrect = npsCrmCaptureCorrect ndrNps
      nrCrmCaptureReason = npsCrmCaptureReason ndrNps
      nrFrontLineRatingReasons = npsFrontLineRatingReasons ndrNps
      nrBackOfficeReasons = npsBackOfficeReasons ndrNps
      nrId = RecordId $ fromSqlKey ndrNpsId
      nrCreatedAt = npsCreatedAt ndrNps
  in NpsRecord{..}
