module User.Model.Types
       ( UserModel(..)
       , UserWithId (..)
       , HasUserWithId (..)
       ) where
import Lens.Micro.Platform (makeClassy)

import Db.Model (User, UserId)
import User.Types (Email, PasswordHash, Uname, UserEdits)

data UserWithId = UserWithId
  { _uiUser :: User
  , _uiId :: UserId
  }
makeClassy ''UserWithId

data UserModel (m :: * -> *) = UserModel
  { umCreateUser :: User -> m (Maybe UserId) -- TODO: should return Either
  , umAllUsers :: m [User]
  , umGetUserByEmail :: Email -> m (Maybe User)
  , umGetUserById :: UserId -> m (Maybe User)
  , umGetUserByName :: Uname -> m (Maybe UserWithId)
  , umUpdateUser :: UserId -> UserEdits -> m User
  , umSetPassword :: UserId -> PasswordHash -> m ()
  }
