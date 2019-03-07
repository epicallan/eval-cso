module User.Model.Types (UserModel(..)) where

import Model (User, UserId)
import User.Types (Email, PasswordHash, UserEdits)

data UserModel m = UserModel
  { usCreateUser :: User -> m UserId
  , usAllUsers :: m [User]
  , usGetUserByEmail :: Email -> m (Maybe User)
  , usGetUserById :: UserId -> m (Maybe User)
  , usUpdateUser :: UserId -> UserEdits -> m User
  , usSetPassword :: UserId -> PasswordHash -> m ()
  }
