module User.Controller
       ( getUserById
       , updateUser
       , listUsers
       , loginUser
       , signupUser
       , generateUser
       , setPassword
       ) where
import Control.Monad.Time (MonadTime, currentTime)
import Data.Ratio ((%))
import Database.Persist.Postgresql (fromSqlKey, toSqlKey)
import Servant
import Servant.Auth.Server
import Test.RandomStrings (randomASCII, randomString')

import Common.Errors (MonadThrowLogger, throwSError)
import Common.Types (Id(..))
import Foundation (HasConfig)
import Model (User(..))
import User.Helper
  (runProtectedAction, throwInvalidUserId, throwUserExists, toUserResponse)
import User.Model.Types (UserModel(..))
import User.Password (hashPassword, validatePassword)
import User.Types
  (Email, HasUserAttrs, Login, Password(..), ServantAuthHeaders, Signup(..),
  UserEdits(..), UserErrors(..), UserResponse(..), email, name, password, role)

getUserById
  :: forall m .(MonadThrowLogger m)
  => UserModel m
  -> Int64
  -> m UserResponse
getUserById usModel uid = getUserById' usModel uid <&> toUserResponse

setPassword
  :: (MonadThrowLogger m, HasConfig r, MonadReader r m)
  => UserModel m
  -> User
  -> Int64
  -> Text
  -> m Id
setPassword usModel logedInUser uid pwd = do
  let userId = toSqlKey uid
  let mkPassword = hashPassword (Password pwd) >>= umSetPassword usModel userId
  user <- getUserById' usModel uid
  runProtectedAction logedInUser (userRole user) mkPassword
  pure $ Id uid

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
  -> Int64
  -> UserEdits
  -> m UserResponse
updateUser usModel logedInUser uid edits = do
  let userId = toSqlKey uid
  user <- getUserById' usModel uid
  let update = toUserResponse <$> umUpdateUser usModel userId edits
  if | userName user == userName logedInUser -> update
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
   dbUser <- umGetUsersByEmail usModel uemail >>= throwInvalidEmail uemail
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
  hpwd <- hashPassword pwd
  utcTime <- currentTime
  mUserId <- umCreateUser usModel $
    User { userRole = userAttrs ^. role
         , userName = userAttrs ^. name
         , userEmail = userAttrs ^. email
         , userPassword = hpwd
         , userCreatedAt = utcTime
         , userUpdatedAt = utcTime
         }
  Id . fromSqlKey <$> mUserId & maybe throwUserExists pure

getUserById' :: MonadThrowLogger m => UserModel m -> Int64 -> m User
getUserById' usModel uid =
  umGetUsersById usModel (toSqlKey uid) >>= maybe (throwInvalidUserId uid) pure
