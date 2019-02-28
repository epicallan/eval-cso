-- Servant API that's composed with the rest of the api
module User.Api
       ( UserApi
       , userServer
       ) where

import Servant
import Servant.Auth.Server

import Common.Types (Id)
import Foundation (App)
import Model (User)
import User.Controller
  ( updateUser, getUserById, listUsers, loginUser
  , signupUser, setPassword, generateUser
  )
import User.Storage.Core (userStorage)
import User.Types (Login, Signup, ServantAuthHeaders, Edits, UserResponse)


type LoginApi =
 "login"
     :> ReqBody '[JSON] Login
     :> PostNoContent '[JSON] ServantAuthHeaders

type SignupApi =
  "signup" :> ReqBody '[JSON] Signup :> Post '[JSON] Id

type ProtectedUserApi =
         Get '[JSON] [UserResponse]
    :<|> ReqBody '[JSON] Edits :> Post '[JSON] Id
    :<|> Capture "id" Int64 :> ReqBody '[JSON] Edits :> Put '[JSON] UserResponse
    :<|> Capture "id" Int64 :> Get '[JSON] UserResponse
    :<|> Capture "id" Int64 :> Capture "password" Text :> Patch '[JSON] Id


type UserApi auths =
  "user" :> Auth auths User :> ProtectedUserApi
  :<|> LoginApi
  :<|> SignupApi

unprotected
  :: CookieSettings
  -> JWTSettings
  -> ServerT LoginApi App
unprotected = loginUser userStorage

protected
  :: AuthResult User
  -> ServerT ProtectedUserApi App
protected (Authenticated user) =
         listUsers userStorage
    :<|> generateUser userStorage user
    :<|> updateUser userStorage user
    :<|> getUserById userStorage
    :<|> setPassword userStorage user

protected _ = throwAll err401

signupApi :: ServerT SignupApi App
signupApi = signupUser userStorage

userServer
  :: CookieSettings
  -> JWTSettings
  -> ServerT (UserApi auths) App
userServer cs jwts = protected :<|> unprotected cs jwts :<|> signupApi

