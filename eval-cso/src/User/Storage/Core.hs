module User.Storage.Core
        ( userStorage
        ) where

import Database.Persist.Postgresql
  (Entity(..), fromSqlKey, insert, selectFirst, selectList, (==.))

import Common.Types (Id(..))
import Config (HasPool)
import Model (EntityField(UserEmail), User, runInDb)

import User.Storage.Types (UserStorage(..))
import User.Types (UserStorageErrors(..))

userStorage :: (MonadIO m, MonadReader r m, HasPool r) => UserStorage m
userStorage = UserStorage
  { usCreateUser = \user -> do
      newUserKey <- runInDb $ insert user
      pure . Id $ fromSqlKey newUserKey

  , usAllUsers = do
      users :: [Entity User] <- runInDb (selectList [] [])
      pure (entityVal <$> users)

  , usGetUserByEmail = \ email ->  do
       mUser :: (Maybe (Entity User)) <- runInDb $ selectFirst [UserEmail ==. email] []
       pure $ case mUser of
         Nothing -> Left $ UserEmailNotFound email
         Just (Entity _ user) -> Right user
  }

-- TODO: userStorageTest
