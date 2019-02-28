module User.Storage.Core
        ( userStorage
        ) where
import Prelude hiding (get)

import Data.Time (getCurrentTime)
import Database.Persist.Postgresql
  ( Entity(..), fromSqlKey, insert, selectFirst, selectList, (==.), (=.), get
  , updateGet, update
  )

import Common.Types (Id(..))
import Foundation (HasPool)
import Model (EntityField(..), User, runInDb)

import User.Storage.Types (UserStorage(..))
import User.Types (UserErrors(..), Edits(..))

userStorage :: (MonadIO m, MonadReader r m, HasPool r) => UserStorage m
userStorage = UserStorage
  { usCreateUser = \user -> do
      newUserKey <- runInDb $ insert user
      pure . Id $ fromSqlKey newUserKey

  , usSetPassword = \userId hpwd -> runInDb $ update userId [UserPassword =. hpwd]

  , usAllUsers = do
      users :: [Entity User] <- runInDb (selectList [] [])
      pure (entityVal <$> users)

  , usGetUserByEmail = \ email ->  do
       mUser :: (Maybe (Entity User)) <- runInDb $ selectFirst [UserEmail ==. email] []
       pure $ case mUser of
         Nothing -> Left $ UserEmailNotFound email
         Just (Entity _ user) -> Right user

   , usGetUserByName = \ name ->  do
       mUser :: (Maybe (Entity User)) <- runInDb $ selectFirst [UserName ==. name] []
       pure $ case mUser of
         Nothing -> Left $ UserNameNotFound name
         Just (Entity _ user) -> Right user

   , usGetUserById = \userId -> do
       mUser :: (Maybe User)<- runInDb $ get userId
       pure $ maybeToRight (UserNotFound . Id $ fromSqlKey userId) mUser

   , usUpdateUser = \ userId Edits{..} -> do
       utcTime <- liftIO getCurrentTime
       runInDb $ updateGet userId
                    [ UserName =. _editsName
                    , UserEmail =. _editsEmail
                    , UserRole =. _editsRole
                    , UserUpdatedAt =. utcTime
                    ]

  }

-- TODO: userStorageTest
