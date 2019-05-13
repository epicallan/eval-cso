module Claim.Controller
       ( getClaims
       , saveClaim
       , saveClaimTypes
       , getClaimTypes
       , deleteClaim
       ) where
import Database.Persist.Postgresql (fromSqlKey, toSqlKey)
import Servant (err400, err401)

import Claim.Model.Types (ClaimModel(..), ClaimScore(..))
import Claim.Types (ClaimRecord(..), ClaimTypeRecord(..), CreateClaim)
import Common.Errors (MonadThrowLogger, eitherSError)
import Common.Types (Id(..), RecordId(..))
import Db.Model (Claim(..), ClaimType(..), User(..))
import User.Helper (runAdminAction, runEvaluatorAction)

getClaims
  :: MonadThrowLogger m
  => ClaimModel m
  -> m [ClaimRecord]
getClaims claimModel = do
  claimScores <- cmGetClaims claimModel
  pure $ toClaimRecord <$> claimScores

saveClaim
  :: MonadThrowLogger m
  => ClaimModel m -> User -> CreateClaim -> m Id
saveClaim claimModel user claim = runEvaluatorAction user $ do
  evaluatorId <- cmGetEvaluatorId claimModel (userName user) >>= eitherSError err401
  cmCreateClaim claimModel evaluatorId claim >>= eitherSError err400

getClaimTypes
  :: MonadThrowLogger m
  => ClaimModel m -> m [ClaimTypeRecord]
getClaimTypes claimModel = do
  claimTypes <- cmGetClaimtypes claimModel
  pure $ toClaimTypeRecord <$> claimTypes

saveClaimTypes
  :: MonadThrowLogger m
  => ClaimModel m -> User -> [ClaimTypeRecord] -> m ()
saveClaimTypes claimModel user claimTypes = runAdminAction user $
  cmCreateClaimtypes claimModel claimTypes

deleteClaim
  :: MonadThrowLogger m
  => ClaimModel m -> User -> Int64 -> m ()
deleteClaim claimModel user cId = runAdminAction user $
  cmDeleteClaim claimModel (toSqlKey cId) -- TODO: check claim ID exists

toClaimTypeRecord :: ClaimType -> ClaimTypeRecord
toClaimTypeRecord ClaimType {..} = ClaimTypeRecord
  { _ctrName = claimTypeName
  , _ctrValue = claimTypeValue
  }

toClaimRecord :: ClaimScore -> ClaimRecord
toClaimRecord ClaimScore{..} =
  let _crEvaluator = userName _csEvaluator
      _crSupervisor = userFullName <$> _csSupervisor
      _crBranch = _csBranch
      _crAgentName = userName _csAgent
      _crComment = claimComment _csClaim
      _crClaimType = claimTypeName _csClaimType
      _crDate = claimUpdatedAt _csClaim
      _crScore = if claimAllParametersMet _csClaim then 100 else 0
      _crWorkflowNumber = claimWorkflowNumber _csClaim
      _crDetails = claimDetails _csClaim
      _crReason = claimReason _csClaim
      _crId = RecordId $  fromSqlKey _csClaimId
  in ClaimRecord{..}
