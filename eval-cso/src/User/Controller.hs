{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ImplicitParams #-}
module User.Controller
       ( getUserByName
       , updateUser
       , listUsers
       , loginUser
       , signupUser
       , deleteUser
       , generateUser
       , generateUser_
       , setPassword
       ) where
import Control.Monad.Time (MonadTime, currentTime)
import Data.ByteString.Lazy as L (ByteString)
import Data.Ratio ((%))
import Database.Persist.Postgresql (fromSqlKey)
import Servant
import Servant.Auth.Server (CookieSettings(..), JWTSettings, makeJWT)
import Test.RandomStrings (randomASCII, randomString')

import Common.Errors (MonadThrowLogger, throwSError)
import Common.Types (Id(..))
import Db.Model (User(..))
import Foundation (HasSettings)
import User.Helper
  (runAdminAction, runProtectedAction, throwInvalidUserName, throwUserExists,
  toUserResponse)
import User.Model.Types
  (HasUserWithId(..), LoggedInUser, LoggedOutUser, SafeUser(..), UserModel(..),
  UserState(LoggedOut), UserWithId(..))
import User.Password (hashPassword, validatePassword)
import User.Types
  (Email, HasCreateUserAttrs, HasUserAttrs, Login, Password(..), UserEdits(..),
  UserErrors(..), UserLoginResponse(..), UserResponse(..), UserToken(..),
  email, fullName, password, role)
import qualified User.Types as U (UserName(..), userName)

type UserEffs m = (MonadThrowLogger m, ?userModel :: UserModel m)

type UserEffsIO r m =
  ( UserEffs m
  , HasSettings r Identity
  , MonadReader r m
  , MonadTime m
  , MonadIO m
  )

getUserByName
  :: UserEffs m
  => U.UserName
  -> m UserResponse
getUserByName uName =
  toUserResponse . view uiUser <$> getUserByName' uName

setPassword
  :: (UserEffs m, HasSettings r Identity, MonadReader r m)
  => LoggedInUser
  -> U.UserName
  -> Password
  -> m Id
setPassword logedInUser uName pwd = do
  (UserWithId user userId) <- getUserByName' uName
  let mkPassword = hashPassword pwd >>= umSetPassword ?userModel userId
  runProtectedAction logedInUser (userRole user) uName mkPassword
  pure . Id $ fromSqlKey userId

generateUser
  :: UserEffsIO r m
  => LoggedInUser
  -> UserEdits
  -> m Id
generateUser logedInUser attrs =
  runProtectedAction
    logedInUser
    (attrs ^. role)
    (attrs ^. U.userName) $ generateUser_  attrs

generateUser_
  :: UserEffsIO r m
  => UserEdits
  -> m Id
generateUser_ attrs = do
  defaultPassword <- liftIO $ randomString' randomASCII (1 % 2) (2 % 3) 7 <&> toText
  createUser attrs $ Password defaultPassword

updateUser
  :: UserEffs m
  => LoggedInUser
  -> U.UserName
  -> UserEdits
  -> m UserResponse
updateUser safeUser@(unSafeUser -> (userName -> loggedInName)) uName edits = do
  (UserWithId user userId) <- getUserByName' uName
  let update = toUserResponse <$> umUpdateUser ?userModel userId edits
  if | uName == loggedInName -> update
     | otherwise -> runProtectedAction safeUser (userRole user) (userName user) update

listUsers
  :: UserEffs m
  => m [UserResponse]
listUsers = fmap toUserResponse <$> umAllUsers ?userModel

-- | on signup everyone is a regular member i.e CSO Agent, admin gives out roles
-- note signup has a default role of CSOAgent from HasRole class
signupUser
  :: (HasSettings r Identity, HasCreateUserAttrs attrs, MonadReader r m, MonadTime m, UserEffs m)
  => attrs
  -> m Id
signupUser attrs = createUser attrs $ attrs ^. password

loginUser
  :: (UserEffs m, MonadIO m)
  => CookieSettings
  -> JWTSettings
  -> Login
  -> m UserLoginResponse
loginUser cs jws loginData = do
   let uemail = loginData ^. email
   dbUser <- umGetUserByEmail ?userModel uemail >>= throwInvalidEmail uemail
   validUser <- if validatePassword (loginData ^. password) (userPassword dbUser)
                   then pure $ SafeUser @'LoggedOut dbUser
                   else throwSError err401 (IncorrectPassword uemail)
   acceptLogin cs jws validUser

   where
     throwInvalidEmail :: MonadThrowLogger m => Email -> Maybe User -> m User
     throwInvalidEmail uEmail = maybe (throwSError err400 $ UserEmailNotFound uEmail) pure

acceptLogin
  :: (MonadThrowLogger m, MonadIO m)
  => CookieSettings
  -> JWTSettings
  -> LoggedOutUser
  -> m UserLoginResponse
acceptLogin cs jws safeUser@(unSafeUser -> user) = do
  ejwt <- liftIO $ makeJWT safeUser jws (cookieExpires cs)
  case ejwt of
    Left err -> throwSError err400 . CookieSetupError $ show err
    Right jwt -> pure . UserLoginResponse
                   (UserToken $ decodeUtf8 @Text @L.ByteString jwt)
                   $ toUserResponse user

createUser
  :: (HasUserAttrs attrs,  MonadTime m, HasSettings r Identity, MonadReader r m, UserEffs m)
  => attrs
  -> Password
  -> m Id
createUser userAttrs pwd = do
  let uName = userAttrs ^. U.userName
  hpwd <- hashPassword pwd
  utcTime <- currentTime
  mUserId <- umCreateUser ?userModel $
    User { userRole = userAttrs ^. role
         , userName = uName
         , userFullName = userAttrs ^. fullName
         , userEmail = userAttrs ^. email
         , userPassword = hpwd
         , userDeleted = Just False
         , userCreatedAt = utcTime
         , userUpdatedAt = utcTime
         }
  Id . fromSqlKey <$> maybe (throwUserExists uName) pure mUserId

deleteUser
  :: UserEffs m
  => LoggedInUser -> U.UserName -> m ()
deleteUser safeUser uName = do
  (UserWithId _ userId) <- getUserByName' uName
  runAdminAction safeUser $ umDeleteUser ?userModel userId

getUserByName' :: UserEffs m =>  U.UserName -> m UserWithId
getUserByName' uName = do
  mUserWithId <- umGetUserByName ?userModel uName
  maybe (throwInvalidUserName uName) pure mUserWithId
