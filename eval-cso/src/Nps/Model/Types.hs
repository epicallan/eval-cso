module Nps.Model.Types
       ( NpsModel(..)
       , NpsDbRecord (..)
       ) where
import Common.Types (Id)
import Db.Model (Nps, NpsId, User, UserId)
import Nps.Types (CreateNps, NpsErrors)
import User.Types (UserName)

data NpsDbRecord = NpsDbRecord
  { ndrNps :: Nps
  , ndrNpsId :: NpsId
  , ndrEvaluator :: User
  , ndrAgent :: User
  , ndrSupervisor :: Maybe User
  }

data NpsModel m = NpsModel
  { nmCreateNps :: UserId -> CreateNps -> m (Either NpsErrors Id)
  , nmGetNpss :: m [NpsDbRecord]
  , nmDeleteNps :: NpsId -> m ()
  , nmGetEvaluatorId :: UserName -> m (Either NpsErrors UserId)
  }
