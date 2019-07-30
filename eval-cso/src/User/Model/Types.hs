{-# LANGUAGE DeriveAnyClass, DerivingStrategies #-}
module User.Model.Types
       ( UserModel(..)
       , UserWithId (..)
       , HasUserWithId (..)
       , SafeUser (..)
       , UserState (..)
       , LoggedInUser
       , LoggedOutUser
       ) where
import Data.Aeson (FromJSON, ToJSON)
import Data.Kind (Type)
import Lens.Micro.Platform (makeClassy)
import Servant.Auth.Server (FromJWT, ToJWT)

import Db.Model (User, UserId)
import User.Types (Email, PasswordHash, UserEdits, UserName)

data UserState = LoggedIn | LoggedOut

newtype SafeUser (a :: UserState) = SafeUser { unSafeUser :: User }
  deriving (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToJWT, FromJWT)


type LoggedInUser = SafeUser 'LoggedIn
type LoggedOutUser = SafeUser 'LoggedOut

data UserWithId = UserWithId
  { _uiUser :: User
  , _uiId :: UserId
  }
makeClassy ''UserWithId

data UserModel (m :: Type -> Type) = UserModel
  { umCreateUser :: User -> m (Maybe UserId) -- TODO: should return Either
  , umAllUsers :: m [User]
  , umGetUserByEmail :: Email -> m (Maybe User)
  , umGetUserById :: UserId -> m (Maybe User)
  , umGetUserByName :: UserName -> m (Maybe UserWithId)
  , umUpdateUser :: UserId -> UserEdits -> m User
  , umSetPassword :: UserId -> PasswordHash -> m ()
  , umDeleteUser :: UserId -> m ()
  }
