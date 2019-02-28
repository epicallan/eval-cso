module User.Storage.Types (UserStorage(..)) where

import Common.Types (Id, Name)
import Model (User, UserId)
import User.Types (Email, UserErrors, Edits, PasswordHash)

data UserStorage m = UserStorage
  { usCreateUser :: User -> m Id
  , usAllUsers :: m [User]
  , usGetUserByEmail :: Email -> m (Either UserErrors User)
  , usGetUserByName :: Name -> m (Either UserErrors User)
  , usGetUserById :: UserId -> m (Either UserErrors User)
  , usUpdateUser :: UserId -> Edits -> m User
  , usSetPassword :: UserId -> PasswordHash -> m ()
  }
