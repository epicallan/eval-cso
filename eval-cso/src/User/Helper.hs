module User.Helper
       ( runProtectedAction
       , toUserResponse
       , throwInvalidUserName
       , throwUserExists
       ) where

import Servant (err400, err404)

import Common.Errors (MonadThrowLogger, throwSError)
import Db.Model (User(..))
import User.Types (Email, Role(..), Uname, UserErrors(..), UserResponse(..))

throwInvalidUserName :: MonadThrowLogger m => Uname -> m a
throwInvalidUserName = throwSError err404 . UserNameNotFound

throwUserNotAuthorized :: MonadThrowLogger m => Email -> m a
throwUserNotAuthorized uemail  =
  throwSError err400 $ UserIsNotAuthrized uemail

throwUserExists :: MonadThrowLogger m => Uname ->  m a
throwUserExists = throwSError err400 . UserExistsError

-- | An admin can do anything, an evaluator can do anything for an agent
-- Agent can only access own account
runProtectedAction
  :: MonadThrowLogger m
  => User -- ^ current logged in user
  -> Role -- ^ role of the user who consumes the action
  -> m a -- ^ action to run
  -> m a
runProtectedAction logedInUser urole action = do
  let uemail = userEmail logedInUser
  case userRole logedInUser of
    Admin -> action
    Evaluator -> if urole == CsoAgent
                    then action
                    else throwUserNotAuthorized uemail
    _   -> throwUserNotAuthorized uemail

toUserResponse :: User -> UserResponse
toUserResponse User{..} =
  UserResponse { urName = userName
               , urFullName = userFullName
               , urEmail = userEmail
               , urRole = userRole
               , urCreatedAt = userCreatedAt
               , urUpdatedAt = userUpdatedAt
               }
