module User.Storage.Types (UserStorage(..)) where

import Common.Types (Id)
import Model (User)
import User.Types (Email, UserStorageErrors)

data UserStorage m = UserStorage
  { usCreateUser :: User -> m Id
  , usAllUsers :: m [User]
  , usGetUserByEmail :: Email -> m (Either UserStorageErrors User)
  }
