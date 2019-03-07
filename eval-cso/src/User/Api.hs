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
  (generateUser, getUserById, listUsers, loginUser, setPassword, signupUser,
  updateUser)
import User.Model.Internal (userModel)
import User.Types (Login, ServantAuthHeaders, Signup, UserEdits, UserResponse)

type LoginApi =
 "login"
     :> ReqBody '[JSON] Login
     :> PostNoContent '[JSON] ServantAuthHeaders

type SignupApi =
  "signup" :> ReqBody '[JSON] Signup :> Post '[JSON] Id

type ProtectedUserApi =
         Get '[JSON] [UserResponse]
    :<|> ReqBody '[JSON] UserEdits :> Post '[JSON] Id
    :<|> Capture "id" Int64 :> ReqBody '[JSON] UserEdits :> Put '[JSON] UserResponse
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
unprotected = loginUser userModel

protected
  :: AuthResult User
  -> ServerT ProtectedUserApi App
protected (Authenticated user) =
         listUsers userModel
    :<|> generateUser userModel user
    :<|> updateUser userModel user
    :<|> getUserById userModel
    :<|> setPassword userModel user

protected _ = throwAll err401

signupApi :: ServerT SignupApi App
signupApi = signupUser userModel

userServer
  :: CookieSettings
  -> JWTSettings
  -> ServerT (UserApi auths) App
userServer cs jwts = protected :<|> unprotected cs jwts :<|> signupApi

