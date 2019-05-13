module Nps.Model.Types
       ( NpsModel(..)
       , NpsDbRecord (..)
       ) where
import Common.Types (Id)
import Db.Model (Nps, NpsId, User, UserId)
import Nps.Types (BranchName, CreateNps, NpsErrors)
import User.Types (UserName)

data NpsDbRecord = NpsDbRecord
  { ndrNps :: Nps
  , ndrNpsId :: NpsId
  , ndrEvaluator :: User
  , ndrAgent :: User
  , ndrSupervisor :: Maybe User
  , ndrBranch :: Maybe BranchName
  }

data NpsModel m = NpsModel
  { nmCreateNps :: UserId -> CreateNps -> m (Either NpsErrors Id)
  , nmGetNpss :: m [NpsDbRecord]
  , nmDeleteNps :: NpsId -> m ()
  , nmGetEvaluatorId :: UserName -> m (Either NpsErrors UserId)
  }
