module Claim.Model.Internal (claimModel) where

import Prelude hiding (on, set, (^.))

import Control.Monad.Time (MonadTime, currentTime)
import Database.Esqueleto hiding ((<&>))

import Claim.Model.Types (ClaimModel(..), ClaimScore(..))
import Claim.Types (ClaimErrors(..), CreateClaim(..), cctTypes)
import qualified Claim.Types as C (ClaimTypeName)
import Common.Types (Id(..))
import Db.Model
import User.Model.Internal (userModel)
import User.Model.Types (HasUserWithId(..), UserModel(..))
import qualified User.Types as U (UserName)

type ExceptClaimM m a = forall r. CanDb m r => ExceptT ClaimErrors m a

claimModel :: forall r m . CanDb m r => ClaimModel m
claimModel = ClaimModel
  { cmCreateClaimtypes = runInDb . putMany <=< traverse mkClaimType . view  cctTypes
  , cmCreateClaim = \userId createClaim -> do
      eClaim <- runExceptT $ mkClaim userId createClaim
      case eClaim of
        Left err -> pure $ Left err
        Right claim -> Right . Id . fromSqlKey <$> runInDb (insert claim)

  , cmGetClaims = do
     claimData :: [(Entity Claim, Entity ClaimType, Entity User, Entity User)] <- runInDb $
       select $
         from $ \(claim `InnerJoin` claimType `InnerJoin` agent `InnerJoin` evaluator
                 ) -> do
            on (evaluator ^. UserId ==. claim ^. ClaimEvaluator)
            on (agent ^. UserId ==. claim ^. ClaimAgent)
            on (claimType ^. ClaimTypeId ==. claim ^. ClaimClaimType)
            return (claim, claimType, agent, evaluator)
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


getClaimTypeId :: C.ClaimTypeName -> ExceptClaimM m ClaimTypeId
getClaimTypeId name = do
  meClaimType :: (Maybe (Entity ClaimType)) <- runInDb $ getBy $ UniqueClaimTypeName name
  ExceptT $ pure $ maybe (Left $ ClaimTypeNotFound name ) (Right . entityKey) meClaimType

mkClaimType :: MonadTime m => C.ClaimTypeName -> m ClaimType
mkClaimType name = do
  utcTime <- currentTime
  pure $ ClaimType
    { claimTypeName = name
    , claimTypeCreatedAt = utcTime
    , claimTypeUpdatedAt = utcTime
    }

toClaimScore :: (Entity Claim, Entity ClaimType, Entity User, Entity User) -> ClaimScore
toClaimScore (eClaim, eClaimType, eAgent, eEvaluator) = ClaimScore
  { _csClaim = entityVal eClaim
  , _csEvaluator = entityVal eEvaluator
  , _csAgent = entityVal eAgent
  , _csClaimType = entityVal eClaimType
  }

mkClaim :: MonadTime m => UserId -> CreateClaim -> ExceptClaimM m Claim
mkClaim claimEvaluator CreateClaim {..} = do
  claimCreatedAt <- currentTime
  claimAgent <- getUserId _ccAgentName
  claimClaimType <- getClaimTypeId _ccClaimType
  let claimAllParametersMet = _ccAllParemetersMet
      claimWorkflowNumber = _ccWorkflowNumber
      claimComment = _ccComment
      claimUpdatedAt = claimCreatedAt
  pure $ Claim {..}
