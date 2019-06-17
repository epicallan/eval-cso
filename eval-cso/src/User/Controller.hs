{-# LANGUAGE TypeApplications #-}
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

getUserByName
  :: forall m .(MonadThrowLogger m)
  => UserModel m
  -> U.UserName
  -> m UserResponse
getUserByName usModel uName =
  toUserResponse . view uiUser <$> getUserByName' usModel uName

setPassword
  :: (MonadThrowLogger m, HasSettings r, MonadReader r m)
  => UserModel m
  -> LoggedInUser
  -> U.UserName
  -> Password
  -> m Id
setPassword usModel logedInUser uName pwd = do
  (UserWithId user userId) <- getUserByName' usModel uName
  let mkPassword = hashPassword pwd >>= umSetPassword usModel userId
  runProtectedAction logedInUser (userRole user) uName mkPassword
  pure . Id $ fromSqlKey userId

generateUser
  :: (HasSettings r, MonadReader r m, MonadTime m, MonadThrowLogger m, MonadIO m)
  => UserModel m
  -> LoggedInUser
  -> UserEdits
  -> m Id
generateUser usModel logedInUser attrs =
  runProtectedAction
    logedInUser
    (attrs ^. role)
    (attrs ^. U.userName) $ generateUser_ usModel attrs

generateUser_
  :: (HasSettings r, MonadReader r m, MonadTime m, MonadThrowLogger m, MonadIO m)
  => UserModel m
  -> UserEdits
  -> m Id
generateUser_ usModel attrs = do
  defaultPassword <- liftIO $ randomString' randomASCII (1 % 2) (2 % 3) 7 <&> toText
  createUser usModel attrs  $ Password defaultPassword

updateUser
  :: MonadThrowLogger m
  => UserModel m
  -> LoggedInUser
  -> U.UserName
  -> UserEdits
  -> m UserResponse
updateUser usModel safeUser@(unSafeUser -> logedInUser) uName edits = do
  (UserWithId user userId) <- getUserByName' usModel uName
  let update = toUserResponse <$> umUpdateUser usModel userId edits
  if | uName == userName logedInUser -> update
     | otherwise -> runProtectedAction safeUser (userRole user) (userName user) update

listUsers
  :: Functor m
  => UserModel m
  -> m [UserResponse]
listUsers us = fmap toUserResponse <$> umAllUsers us

-- | on signup everyone is a regular member i.e CSO Agent, admin gives out roles
-- note signup has a default role of CSOAgent from HasRole class
signupUser
  :: (HasSettings r, HasCreateUserAttrs attrs, MonadReader r m, MonadTime m, MonadThrowLogger m)
  => UserModel m
  -> attrs
  -> m Id
signupUser usModel attrs = createUser usModel attrs $ attrs ^. password

loginUser
  :: (MonadThrowLogger m, MonadIO m)
  => UserModel m
  -> CookieSettings
  -> JWTSettings
  -> Login
  -> m UserLoginResponse
loginUser usModel cs jws loginData = do
   let uemail = loginData ^. email
   dbUser <- umGetUserByEmail usModel uemail >>= throwInvalidEmail uemail
   validUser <- if validatePassword (loginData ^. password) (userPassword dbUser)
                   then pure $ SafeUser @'LoggedOut dbUser
                   else throwSError err401 (IncorrectPassword uemail)
   acceptLogin cs jws validUser

   where
     throwInvalidEmail :: MonadThrowLogger m=> Email -> Maybe User -> m User
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
  :: (HasUserAttrs attrs,  MonadTime m, HasSettings r, MonadReader r m, MonadThrowLogger m)
  => UserModel m
  -> attrs
  -> Password
  -> m Id
createUser usModel userAttrs pwd = do
  let uName = userAttrs ^. U.userName
  hpwd <- hashPassword pwd
  utcTime <- currentTime
  mUserId <- umCreateUser usModel $
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
  :: MonadThrowLogger m
  => UserModel m -> LoggedInUser -> U.UserName -> m ()
deleteUser usModel safeUser uName = do
  (UserWithId _ userId) <- getUserByName' usModel uName
  runAdminAction safeUser $ umDeleteUser usModel userId

getUserByName' :: MonadThrowLogger m => UserModel m -> U.UserName -> m UserWithId
getUserByName' usModel uName = do
  mUserWithId <- umGetUserByName usModel uName
  maybe (throwInvalidUserName uName) pure mUserWithId
