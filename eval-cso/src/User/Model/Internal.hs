module User.Model.Internal
        ( userModel
        ) where
import Prelude hiding (get)

import Data.Time (getCurrentTime)
import Database.Persist.Postgresql
  (Entity(..), entityVal, get, insert, selectFirst, selectList, update,
  updateGet, (=.), (==.))

import Foundation (HasPool)
import Model
import User.Model.Types (UserModel(..))
import User.Types (UserEdits(..))

userModel :: (MonadIO m, MonadReader r m, HasPool r) => UserModel m
userModel = UserModel
  { usCreateUser = runInDb . insert

  , usSetPassword = \userId hpwd -> runInDb $ update userId [UserPassword =. hpwd]

  , usAllUsers = do
      users :: [Entity User] <- runInDb (selectList [] [])
      pure (entityVal <$> users)

  , usGetUserByEmail = \email ->  do
       mUser :: (Maybe (Entity User)) <- runInDb $ selectFirst [UserEmail ==. email] []
       pure $ entityVal <$> mUser

   , usGetUserById = runInDb . get

   , usUpdateUser = \userId UserEdits{..} -> do
       utcTime <- liftIO getCurrentTime
       runInDb $ updateGet userId
                    [ UserName =. _userEditsName
                    , UserEmail =. _userEditsEmail
                    , UserRole =. _userEditsRole
                    , UserUpdatedAt =. utcTime
                    ]

  }

-- TODO: userModelTest
