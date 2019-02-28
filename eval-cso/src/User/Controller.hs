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
import Database.Persist.Postgresql (toSqlKey)
import Servant
import Servant.Auth.Server

import Common.Types (Id(..))
import Common.Errors (eitherSError, throwSError)
import Foundation (HasConfig)
import Model (User (..))
import User.Password
  (validatePassword, hashPassword)
import User.Storage.Types (UserStorage(..))
import User.Types
  ( Email(..), Login, Signup(..), Role(..), UserErrors(..)
  , ServantAuthHeaders, Edits(..), Password (..), UserResponse (..)
  , HasUserAttrs, role, name, email, password
  )

getUserById
  :: forall m .(MonadThrow m)
  => UserStorage m
  -> Int64
  -> m UserResponse
getUserById us userId = do
  eUser <- usGetUserById us (toSqlKey userId)
  eitherSError err400 $ second toUserResponse eUser

setPassword
  :: (MonadThrow m, HasConfig r, MonadReader r m)
  => UserStorage m
  -> User
  -> Int64
  -> Text
  -> m Id
setPassword us logedInUser uid pwd = do
  let userId = toSqlKey uid
  let mkPassword = hashPassword (Password pwd) >>= usSetPassword us userId

  userForPassword <- usGetUserById us userId >>= eitherSError err400
  runProtectedAction mkPassword logedInUser $ userRole userForPassword
  pure $ Id uid

generateUser
  :: (HasConfig r, MonadReader r m, MonadTime m, MonadThrow m)
  => UserStorage m
  -> User
  -> Edits
  -> m Id
generateUser us logedInUser attrs =
  let defaultPassword = Password "TODO: make a random string with name as seed"
      createU = createUser us attrs defaultPassword
  in runProtectedAction createU logedInUser $ attrs ^. role

updateUser
  :: (MonadThrow m)
  => UserStorage m
  -> User
  -> Int64
  -> Edits
  -> m UserResponse
updateUser us logedInUser uid edits = do
  let userId = toSqlKey uid
  user <- eitherSError err401 =<< usGetUserById us userId
  let update = toUserResponse <$> usUpdateUser us userId edits
  if | userName user == userName logedInUser -> update
     | otherwise -> runProtectedAction update logedInUser $ userRole user

listUsers
  :: Functor m
  => UserStorage m
  -> m [UserResponse]
listUsers us = fmap toUserResponse <$> usAllUsers us

-- on signup everyone is a regular member, admin gives out roles
signupUser
  :: (HasConfig r, MonadReader r m, MonadTime m)
  => UserStorage m
  -> Signup
  -> m Id
signupUser us attrs = createUser us attrs $ attrs ^. password

loginUser
  :: (MonadThrow m, MonadIO m)
  => UserStorage m
  -> CookieSettings
  -> JWTSettings
  -> Login
  -> m ServantAuthHeaders
loginUser us cs jws loginData = do
   let uemail = loginData ^. email
   dbUser <- usGetUserByEmail us uemail >>=  eitherSError err400
   validUser <- if validatePassword (loginData ^. password) (userPassword dbUser)
                   then pure dbUser
                   else throwSError err401 (IncorrectPassword uemail)
   mApplyCookies <- liftIO $ acceptLogin cs jws validUser
   case mApplyCookies of
     Nothing -> throwSError err400 (CookieSetupError uemail)
     Just applyCookies -> pure $ applyCookies NoContent

---------------------------------------
 -- Utilities
---------------------------------------
createUser
  :: (HasUserAttrs attrs,  MonadTime m, HasConfig r, MonadReader r m)
  => UserStorage m
  -> attrs
  -> Password
  -> m Id
createUser us userAttrs pwd = do
  hpwd <- hashPassword pwd
  utcTime <- currentTime
  usCreateUser us $
    User { userRole = userAttrs ^. role
         , userName = userAttrs ^. name
         , userEmail = userAttrs ^. email
         , userPassword = hpwd
         , userCreatedAt = utcTime
         , userUpdatedAt = utcTime
         }

throwUserNotAuthorized :: MonadThrow m => Email -> m a
throwUserNotAuthorized uemail  =
  throwSError err400 $ UserIsNotAuthrized uemail

runProtectedAction
  :: (MonadThrow m)
  => m a
  -> User
  -> Role -- ^ role of the user who consumes the action
  -> m a
runProtectedAction action logedInUser urole = do
  let uemail = userEmail logedInUser
  case userRole logedInUser of
    Admin -> action
    Evaluator -> if urole == Member
                    then action
                    else throwUserNotAuthorized uemail
    _   -> throwUserNotAuthorized uemail

toUserResponse :: User -> UserResponse
toUserResponse User{..} =
  UserResponse { urName = userName
               , urEmail = userEmail
               , urRole = userRole
               , urCreatedAt = userCreatedAt
               , urUpdatedAt = userUpdatedAt
               }

