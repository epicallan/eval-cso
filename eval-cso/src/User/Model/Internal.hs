module User.Model.Internal
        ( userModel
        ) where
import Prelude hiding (get)

import Control.Monad.Time (currentTime)
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
      users :: [Entity User] <- runInDb (selectList [UserDeleted ==. Just False] [])
      pure (entityVal <$> users)

  , umGetUserByEmail = \email ->  do
       mUser :: (Maybe (Entity User)) <- runInDb $ selectFirst [UserEmail ==. email] []
       pure $ entityVal <$> mUser

  , umGetUserByName = \name ->  do
       mUser :: (Maybe (Entity User)) <- runInDb $ getBy $ UniqueUserName name
       pure $ case mUser of
         Nothing -> Nothing
         Just (Entity userId user) -> Just $ UserWithId user userId

  , umDeleteUser = \userId -> runInDb $ update userId [UserDeleted =. Just True]

   , umGetUserById = runInDb . get

   , umUpdateUser = \userId UserEdits{..} -> do
       utcTime <- currentTime
       runInDb $ updateGet userId
                    [ UserName =. _userEditsUserName
                    , UserFullName =. _userEditsFullName
                    , UserEmail =. _userEditsEmail
                    , UserRole =. _userEditsRole
                    , UserUpdatedAt =. utcTime
                    ]

  }

-- TODO: userModelTest
