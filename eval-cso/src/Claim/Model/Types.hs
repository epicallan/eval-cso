module Claim.Model.Types
       ( ClaimModel(..)
       , ClaimScore (..)
       ) where
import Lens.Micro.Platform (makeClassy)

import Claim.Types (ClaimErrors, CreateClaim, CreateClaimTypes)
import Common.Types (Id)
import Db.Model (Claim, ClaimType, User, UserId)
import User.Types (UserName)

data ClaimScore = ClaimScore
  { _csClaim :: Claim
  , _csEvaluator :: User
  , _csAgent :: User
  , _csClaimType :: ClaimType
  }

makeClassy ''ClaimScore

data ClaimModel m = ClaimModel
  { cmCreateClaimtypes :: CreateClaimTypes -> m ()
  , cmCreateClaim :: UserId -> CreateClaim -> m (Either ClaimErrors Id)
  , cmGetClaims :: m [ClaimScore]
  , cmGetClaimtypes :: m [ClaimType]
  , cmGetEvaluatorId :: UserName -> m (Either ClaimErrors UserId)
  }
