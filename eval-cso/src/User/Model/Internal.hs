module User.Model.Internal
        ( userModel
        ) where
import Prelude hiding (get)

import Data.Time (getCurrentTime)
import Database.Persist.Postgresql
  (Entity(..), entityVal, get, getBy, insertUnique, selectFirst, selectList,
  update, updateGet, (=.), (==.))

import Db.Model
import User.Model.Types (UserModel(..), UserWithId(..))
import User.Types (UserEdits(..))

userModel :: forall r m . CanDb m r => UserModel m
userModel = UserModel
  { umCreateUser = runInDb . insertUnique

  , umSetPassword = \userId hpwd -> runInDb $ update userId [UserPassword =. hpwd]

  , umAllUsers = do
      users :: [Entity User] <- runInDb (selectList [] [])
      pure (entityVal <$> users)

  , umGetUserByEmail = \email ->  do
       mUser :: (Maybe (Entity User)) <- runInDb $ selectFirst [UserEmail ==. email] []
       pure $ entityVal <$> mUser

  , umGetUserByName = \name ->  do
       mUser :: (Maybe (Entity User)) <- runInDb $ getBy $ UniqueUserName name
       pure $ case mUser of
         Nothing -> Nothing
         Just (Entity userId user) -> Just $ UserWithId user userId

   , umGetUserById = runInDb . get

   , umUpdateUser = \userId UserEdits{..} -> do
       utcTime <- liftIO getCurrentTime
       runInDb $ updateGet userId
                    [ UserName =. _userEditsName
                    , UserEmail =. _userEditsEmail
                    , UserRole =. _userEditsRole
                    , UserUpdatedAt =. utcTime
                    ]

  }

-- TODO: userModelTest
