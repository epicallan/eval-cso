-- Servant API that's composed with the rest of the api
module User.Api
       ( UserApi
       , userServer
       ) where

import Servant

import Common.Types (Id)
import Config (AppEffs)
import Model (User)
import User.Controller (createUser, getUserByEmail, listUsers)
import User.Storage.Core (userStorage)

type UserApi =
         Get '[JSON] [User]
    :<|> ReqBody '[JSON] User :> Post '[JSON] Id
    :<|> Capture "email" Text :> Get '[JSON] User

userServer :: AppEffs r m => ServerT UserApi m
userServer =
         listUsers userStorage
    :<|> createUser userStorage
    :<|> getUserByEmail userStorage

