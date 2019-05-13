module Claim.Model.Types
       ( ClaimModel(..)
       , ClaimScore (..)
       ) where
import Lens.Micro.Platform (makeClassy)

import Claim.Types (ClaimErrors, CreateClaim, ClaimTypeRecord, BranchName)
import Common.Types (Id)
import Db.Model (Claim, ClaimType, User, UserId, ClaimId)
import User.Types (UserName)

data ClaimScore = ClaimScore
  { _csClaim :: Claim
  , _csClaimId :: ClaimId
  , _csEvaluator :: User
  , _csAgent :: User
  , _csSupervisor :: Maybe User
  , _csBranch :: Maybe BranchName
  , _csClaimType :: ClaimType
  }

makeClassy ''ClaimScore

data ClaimModel m = ClaimModel
  { cmCreateClaimtypes :: [ClaimTypeRecord] -> m ()
  , cmCreateClaim :: UserId -> CreateClaim -> m (Either ClaimErrors Id)
  , cmGetClaims :: m [ClaimScore]
  , cmGetClaimtypes :: m [ClaimType]
  , cmGetEvaluatorId :: UserName -> m (Either ClaimErrors UserId)
  , cmDeleteClaim :: ClaimId -> m ()
  }
