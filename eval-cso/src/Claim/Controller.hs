module Claim.Controller
       ( getClaims
       , saveClaim
       , saveClaimTypes
       , getClaimTypes
       ) where
import Servant (err400, err401)

import Claim.Model.Types (ClaimModel(..), ClaimScore(..))
import Claim.Types
  (ClaimErrors(..), ClaimRecord(..), ClaimTypeRecord(..), CreateClaim)
import Common.Errors (MonadThrowLogger, eitherSError, throwSError)
import Common.Types (Id(..))
import Db.Model (Claim(..), ClaimType(..), User(..))
import User.Types (Role(..))

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
saveClaim claimModel user claim = protectedAction user $ do
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
saveClaimTypes claimModel user claimTypes =
  adminAction user $ cmCreateClaimtypes claimModel claimTypes


adminAction :: MonadThrowLogger m => User -> m a -> m a
adminAction User{..} action = case userRole of
  Admin     -> action
  _         -> throwSError err401 $ ActionIsForAdmins userName

protectedAction :: MonadThrowLogger m => User -> m a -> m a
protectedAction User{..} action = case userRole of
  Evaluator -> action
  Admin     -> action
  _         -> throwSError err401 $ ActionIsForEvaluators userName

toClaimTypeRecord :: ClaimType -> ClaimTypeRecord
toClaimTypeRecord ClaimType {..} = ClaimTypeRecord
  { _ctrName = claimTypeName
  , _ctrValue = claimTypeValue
  }

toClaimRecord :: ClaimScore -> ClaimRecord
toClaimRecord ClaimScore{..} =
  let _crEvaluator = userName _csEvaluator
      _crAgentName = userName _csAgent
      _crComment = claimComment _csClaim
      _crClaimType = claimTypeName _csClaimType
      _crDate = claimUpdatedAt _csClaim
      _crScore = if claimAllParametersMet _csClaim then 100 else 0
      _crWorkflowNumber = claimWorkflowNumber _csClaim
      _crDetails = claimDetails _csClaim
  in ClaimRecord{..}
