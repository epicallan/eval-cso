module User.Model.Types (UserModel(..)) where

import Model (User, UserId)
import User.Types (Email, PasswordHash, UserEdits)

data UserModel m = UserModel
  { umCreateUser :: User -> m UserId
  , umAllUsers :: m [User]
  , umGetUsersByEmail :: Email -> m (Maybe User)
  , umGetUsersById :: UserId -> m (Maybe User)
  , umUpdateUser :: UserId -> UserEdits -> m User
  , umSetPassword :: UserId -> PasswordHash -> m ()
  }
