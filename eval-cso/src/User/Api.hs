-- Servant API that's composed with the rest of the api
module User.Api
       ( UserApi
       , userServer
       ) where

import Servant
import Servant.Auth.Server

import Common.Types (Id)
import Foundation (App)
import User.Controller
  (deleteUser, generateUser, getUserByName, listUsers, loginUser, setPassword,
  signupUser, updateUser)
import User.Model.Internal (userModel)
import User.Model.Types (LoggedInUser)
import User.Types
  (Login, Password, Signup, UserEdits, UserLoginResponse, UserName,
  UserResponse)

type LoginApi =
 "login"
     :> ReqBody '[JSON] Login
     :> Post '[JSON] UserLoginResponse

type SignupApi =
  "signup" :> ReqBody '[JSON] Signup :> Post '[JSON] Id

type ProtectedUserApi =
       Get '[JSON] [UserResponse]
  :<|> ReqBody '[JSON] UserEdits :> Post '[JSON] Id
  :<|> Capture "userName" UserName :> ReqBody '[JSON] UserEdits :> Put '[JSON] UserResponse
  :<|> Capture "userName" UserName :> Get '[JSON] UserResponse
  :<|> Capture "userName" UserName :> Capture "password" Password :> Patch '[JSON] Id
  :<|> Capture "userName" UserName :> Delete '[JSON] ()


type UserApi auths = "users" :>
  (    LoginApi
  :<|> SignupApi
  :<|> Auth auths LoggedInUser :> ProtectedUserApi
  )


loginHandler
  :: CookieSettings
  -> JWTSettings
  -> ServerT LoginApi App
loginHandler = let ?userModel = userModel in loginUser

protected
  :: AuthResult LoggedInUser
  -> ServerT ProtectedUserApi App
protected (Authenticated user) =
    let ?userModel = userModel in
         listUsers
    :<|> generateUser user
    :<|> updateUser  user
    :<|> getUserByName
    :<|> setPassword  user
    :<|> deleteUser user

protected _ = throwAll err401

signupHandler :: ServerT SignupApi App
signupHandler = let ?userModel = userModel in signupUser

userServer
  :: CookieSettings
  -> JWTSettings
  -> ServerT (UserApi auths) App
userServer cs jwts =loginHandler cs jwts :<|> signupHandler :<|> protected
