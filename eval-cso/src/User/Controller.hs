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
import Database.Persist.Postgresql (fromSqlKey, toSqlKey)
import Servant
import Servant.Auth.Server

import Common.Errors (throwSError)
import Common.Types (Id(..))
import Foundation (HasConfig)
import Model (User(..))
import User.Helper (runProtectedAction, throwInvalidUserId, toUserResponse)
import User.Model.Types (UserModel(..))
import User.Password (hashPassword, validatePassword)
import User.Types
  (Email, HasUserAttrs, Login, Password(..), ServantAuthHeaders, Signup(..),
  UserEdits(..), UserErrors(..), UserResponse(..), email, name, password, role)

getUserById
  :: forall m .(MonadThrow m)
  => UserModel m
  -> Int64
  -> m UserResponse
getUserById us uid = getUserById' us uid <&> toUserResponse

setPassword
  :: (MonadThrow m, HasConfig r, MonadReader r m)
  => UserModel m
  -> User
  -> Int64
  -> Text
  -> m Id
setPassword us logedInUser uid pwd = do
  let userId = toSqlKey uid
  let mkPassword = hashPassword (Password pwd) >>= usSetPassword us userId
  user <- getUserById' us uid
  runProtectedAction logedInUser (userRole user) mkPassword
  pure $ Id uid

generateUser
  :: (HasConfig r, MonadReader r m, MonadTime m, MonadThrow m)
  => UserModel m
  -> User
  -> UserEdits
  -> m Id
generateUser us logedInUser attrs =
  let defaultPassword = Password "TODO: make a random string with name as seed"
      createU = createUser us attrs defaultPassword
  in runProtectedAction logedInUser (attrs ^. role) createU

updateUser
  :: (MonadThrow m)
  => UserModel m
  -> User
  -> Int64
  -> UserEdits
  -> m UserResponse
updateUser us logedInUser uid edits = do
  let userId = toSqlKey uid
  user <- getUserById' us uid
  let update = toUserResponse <$> usUpdateUser us userId edits
  if | userName user == userName logedInUser -> update
     | otherwise -> runProtectedAction logedInUser (userRole user) update

listUsers
  :: Functor m
  => UserModel m
  -> m [UserResponse]
listUsers us = fmap toUserResponse <$> usAllUsers us

-- on signup everyone is a regular member, admin gives out roles
signupUser
  :: (HasConfig r, MonadReader r m, MonadTime m)
  => UserModel m
  -> Signup
  -> m Id -- TODO: should return user
signupUser us attrs = createUser us attrs $ attrs ^. password

loginUser
  :: (MonadThrow m, MonadIO m)
  => UserModel m
  -> CookieSettings
  -> JWTSettings
  -> Login
  -> m ServantAuthHeaders
loginUser us cs jws loginData = do
   let uemail = loginData ^. email
   dbUser <- usGetUserByEmail us uemail >>= throwInvalidEmail uemail
   validUser <- if validatePassword (loginData ^. password) (userPassword dbUser)
                   then pure dbUser
                   else throwSError err401 (IncorrectPassword uemail)
   mApplyCookies <- liftIO $ acceptLogin cs jws validUser
   case mApplyCookies of
     Nothing -> throwSError err400 (CookieSetupError uemail)
     Just applyCookies -> pure $ applyCookies NoContent

   where
     throwInvalidEmail :: MonadThrow m => Email -> Maybe User -> m User
     throwInvalidEmail uEmail = maybe (throwSError err400 $ UserEmailNotFound uEmail) pure

---------------------------------------
 -- Utilities
---------------------------------------
createUser
  :: (HasUserAttrs attrs,  MonadTime m, HasConfig r, MonadReader r m)
  => UserModel m
  -> attrs
  -> Password
  -> m Id
createUser us userAttrs pwd = do
  hpwd <- hashPassword pwd
  utcTime <- currentTime
  userId <- usCreateUser us $
    User { userRole = userAttrs ^. role
         , userName = userAttrs ^. name
         , userEmail = userAttrs ^. email
         , userPassword = hpwd
         , userCreatedAt = utcTime
         , userUpdatedAt = utcTime
         }
  pure . Id . fromSqlKey $ userId

getUserById' :: MonadThrow m => UserModel m -> Int64 -> m User
getUserById' us uid =
  usGetUserById us (toSqlKey uid) >>= maybe (throwInvalidUserId uid) pure
