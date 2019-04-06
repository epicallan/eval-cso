module User.Controller
       ( getUserByName
       , updateUser
       , listUsers
       , loginUser
       , signupUser
       , generateUser
       , setPassword
       ) where
import Control.Monad.Time (MonadTime, currentTime)
import Data.Ratio ((%))
import Database.Persist.Postgresql (fromSqlKey)
import Servant
import Servant.Auth.Server
import Test.RandomStrings (randomASCII, randomString')

import Common.Errors (MonadThrowLogger, throwSError)
import Common.Types (Id(..))
import Foundation (HasConfig)
import Model (User(..))
import User.Helper
  (runProtectedAction, throwInvalidUserName, throwUserExists, toUserResponse)
import User.Model.Types (HasUserWithId(..), UserModel(..), UserWithId(..))
import User.Password (hashPassword, validatePassword)
import User.Types
  (Email, HasUserAttrs, Login, Password(..), ServantAuthHeaders, Signup(..),
  Uname(..), UserEdits(..), UserErrors(..), UserResponse(..), email, name,
  password, role)

getUserByName
  :: forall m .(MonadThrowLogger m)
  => UserModel m
  -> Text -- ^ userName
  -> m UserResponse
getUserByName usModel nameTxt =
  let uName = Uname nameTxt
  in toUserResponse . view uiUser <$> getUserByName' usModel uName

setPassword
  :: (MonadThrowLogger m, HasConfig r, MonadReader r m)
  => UserModel m
  -> User
  -> Text -- ^ userName
  -> Text
  -> m Id
setPassword usModel logedInUser nameTxt pwd = do
  let uName = Uname nameTxt
  (UserWithId user userId) <- getUserByName' usModel uName
  let mkPassword = hashPassword (Password pwd) >>= umSetPassword usModel userId
  runProtectedAction logedInUser (userRole user) mkPassword
  pure . Id $ fromSqlKey userId

generateUser
  :: (HasConfig r, MonadReader r m, MonadTime m, MonadThrowLogger m, MonadIO m)
  => UserModel m
  -> User
  -> UserEdits
  -> m Id
generateUser usModel logedInUser attrs = do
  defaultPassword <- liftIO $ randomString' randomASCII (1 % 2) (2 % 3) 7 <&> toText
  let createU = createUser usModel attrs  $ Password defaultPassword
  runProtectedAction logedInUser (attrs ^. role) createU

updateUser
  :: MonadThrowLogger m
  => UserModel m
  -> User
  -> Text -- ^ userName
  -> UserEdits
  -> m UserResponse
updateUser usModel logedInUser nameTxt edits = do
  let uName = Uname nameTxt
  (UserWithId user userId) <- getUserByName' usModel uName
  let update = toUserResponse <$> umUpdateUser usModel userId edits
  if | uName == userName logedInUser -> update
     | otherwise -> runProtectedAction logedInUser (userRole user) update

listUsers
  :: Functor m
  => UserModel m
  -> m [UserResponse]
listUsers us = fmap toUserResponse <$> umAllUsers us

-- | on signup everyone is a regular member i.e CSO Agent, admin gives out roles
-- note signup has a default role of CsoAgent from HasRole class
signupUser
  :: (HasConfig r, MonadReader r m, MonadTime m, MonadThrowLogger m)
  => UserModel m
  -> Signup
  -> m Id
signupUser usModel attrs = createUser usModel attrs $ attrs ^. password

loginUser
  :: (MonadThrowLogger m, MonadIO m)
  => UserModel m
  -> CookieSettings
  -> JWTSettings
  -> Login
  -> m ServantAuthHeaders
loginUser usModel cs jws loginData = do
   let uemail = loginData ^. email
   dbUser <- umGetUserByEmail usModel uemail >>= throwInvalidEmail uemail
   validUser <- if validatePassword (loginData ^. password) (userPassword dbUser)
                   then pure dbUser
                   else throwSError err401 (IncorrectPassword uemail)
   mApplyCookies <- liftIO $ acceptLogin cs jws validUser
   case mApplyCookies of
     Nothing -> throwSError err400 (CookieSetupError uemail)
     Just applyCookies -> pure $ applyCookies NoContent

   where
     throwInvalidEmail :: MonadThrowLogger m=> Email -> Maybe User -> m User
     throwInvalidEmail uEmail = maybe (throwSError err400 $ UserEmailNotFound uEmail) pure

---------------------------------------
 -- Utilities
---------------------------------------

createUser
  :: (HasUserAttrs attrs,  MonadTime m, HasConfig r, MonadReader r m, MonadThrowLogger m)
  => UserModel m
  -> attrs
  -> Password
  -> m Id
createUser usModel userAttrs pwd = do
  let uName = userAttrs ^. name
  hpwd <- hashPassword pwd
  utcTime <- currentTime
  mUserId <- umCreateUser usModel $
    User { userRole = userAttrs ^. role
         , userName = uName
         , userEmail = userAttrs ^. email
         , userPassword = hpwd
         , userCreatedAt = utcTime
         , userUpdatedAt = utcTime
         }
  Id . fromSqlKey <$> maybe (throwUserExists uName) pure mUserId

getUserByName' :: MonadThrowLogger m => UserModel m -> Uname -> m UserWithId
getUserByName' usModel uName = do
  mUserWithId <- umGetUserByName usModel uName
  maybe (throwInvalidUserName uName) pure mUserWithId
