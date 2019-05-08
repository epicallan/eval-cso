module Nps.Model.Types
       ( NpsModel(..)
       , NpsDbRecord (..)
       ) where
import Common.Types (Id)
import Db.Model (Nps, User, UserId)
import Nps.Types (CreateNps, NpsErrors, BranchName)
import User.Types (UserName)

data NpsDbRecord = NpsDbRecord
  { ndrNps :: Nps
  , ndrEvaluator :: User
  , ndrAgent :: User
  , ndrSupervisor :: Maybe User
  , ndrBranch :: Maybe BranchName
  }

data NpsModel m = NpsModel
  { nmCreateNps :: UserId -> CreateNps -> m (Either NpsErrors Id)
  , nmGetNpss :: m [NpsDbRecord]
  , nmGetEvaluatorId :: UserName -> m (Either NpsErrors UserId)
  }
