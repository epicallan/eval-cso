module Claim.Model.Internal (claimModel) where

import Prelude hiding (on, set, (^.))

import Control.Monad.Time (MonadTime, currentTime)
import Database.Esqueleto hiding ((<&>))

import Claim.Model.Types (ClaimModel(..), ClaimScore(..))
import Claim.Types (ClaimErrors(..), ClaimTypeRecord(..), CreateClaim(..))
import qualified Claim.Types as C (ClaimTypeValue)
import Common.Types (Id(..))
import Db.Model
import User.Model.Internal (userModel)
import User.Model.Types (HasUserWithId(..), UserModel(..))
import qualified User.Types as U (UserName)

type ExceptClaimM m a = forall r. CanDb m r => ExceptT ClaimErrors m a

type ClaimScoreTuple =
  ( Entity Claim
  , Entity ClaimType
  , Entity User
  , Maybe (Entity Branch)
  , Maybe (Entity User)
  , Entity User
  )

claimModel :: forall r m . CanDb m r => ClaimModel m
claimModel = ClaimModel
  { cmCreateClaimtypes = runInDb . putMany <=< traverse mkClaimType
  , cmCreateClaim = \userId createClaim -> do
      eClaim <- runExceptT $ mkClaim userId createClaim
      case eClaim of
        Left err -> pure $ Left err
        Right claim -> Right . Id . fromSqlKey <$> runInDb (insert claim)

  , cmGetClaims = do
     claimData :: [ClaimScoreTuple] <- runInDb $
       select $
         from $ \(claim `InnerJoin` claimType `InnerJoin` agent
                 `InnerJoin` agentProfile `InnerJoin` branch
                 `InnerJoin` supervisor `InnerJoin` evaluator
                 ) -> do
            on (evaluator ^. UserId ==. claim ^. ClaimEvaluator)
            on (agentProfile ^. AgentSupervisorId ==. supervisor ?. UserId)
            on (agentProfile ^. AgentBranch ==. branch ?. BranchId)
            on (agentProfile ^. AgentUserId ==. claim ^. ClaimAgent)
            on (agent ^. UserId ==. claim ^. ClaimAgent)
            on (claimType ^. ClaimTypeId ==. claim ^. ClaimClaimType)
            return (claim, claimType, agent, branch, supervisor, evaluator)
     return $ toClaimScore <$> claimData

  , cmGetClaimtypes = do
      claimTypes :: [Entity ClaimType] <- runInDb $ select $ from $ \claimType -> return claimType
      pure $ entityVal <$> claimTypes

  , cmGetEvaluatorId = \uname -> do
      mUserWithId <- umGetUserByName userModel uname
      pure $ maybe (Left $ UserNameNotFound uname) (Right . view uiId) mUserWithId
  }


getUserId :: U.UserName -> ExceptClaimM m UserId
getUserId name = do
  mUserWithId <- umGetUserByName userModel name
  ExceptT $ pure . maybe (Left $ UserNameNotFound name) (Right . view uiId) $ mUserWithId


getClaimTypeId :: C.ClaimTypeValue -> ExceptClaimM m ClaimTypeId
getClaimTypeId value = do
  meClaimType :: (Maybe (Entity ClaimType)) <- runInDb $ getBy $ UniqueClaimTypeValue value
  ExceptT $ pure $ maybe (Left $ ClaimTypeNotFound value ) (Right . entityKey) meClaimType

mkClaimType :: MonadTime m => ClaimTypeRecord -> m ClaimType
mkClaimType ClaimTypeRecord{..} = do
  utcTime <- currentTime
  pure $ ClaimType
    { claimTypeName = _ctrName
    , claimTypeValue = _ctrValue
    , claimTypeCreatedAt = utcTime
    , claimTypeUpdatedAt = utcTime
    }

toClaimScore :: ClaimScoreTuple -> ClaimScore
toClaimScore (eClaim, eClaimType, eAgent, mBranch, mSupervisor, eEvaluator) = ClaimScore
  { _csClaim = entityVal eClaim
  , _csEvaluator = entityVal eEvaluator
  , _csAgent = entityVal eAgent
  , _csClaimType = entityVal eClaimType
  , _csSupervisor = entityVal <$> mSupervisor
  , _csBranch = branchName . entityVal <$> mBranch
  }

mkClaim :: MonadTime m => UserId -> CreateClaim -> ExceptClaimM m Claim
mkClaim claimEvaluator CreateClaim {..} = do
  claimAgent <- getUserId _ccAgentName
  claimClaimType <- getClaimTypeId _ccClaimType
  let claimAllParametersMet = _ccAllParametersMet
      claimWorkflowNumber = _ccWorkflowNumber
      claimReason = _ccReason
      claimDetails = _ccDetails
      claimComment = _ccComment
      claimCreatedAt = _ccDate
      claimUpdatedAt = _ccDate
  pure $ Claim {..}
