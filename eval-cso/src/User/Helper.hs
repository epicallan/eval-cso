module User.Helper
       ( runProtectedAction
       , toUserResponse
       , throwInvalidUserId
       , throwUserExists
       ) where

import Servant (err400, err404)

import Common.Errors (throwSError)
import Common.Types (Id(..))
import Model (User(..))
import User.Types (Email, Role(..), UserErrors(..), UserResponse(..))

throwInvalidUserId :: MonadThrow m => Int64 -> m a
throwInvalidUserId uid = throwSError err404 $ UserNotFound $ Id uid

throwUserNotAuthorized :: MonadThrow m => Email -> m a
throwUserNotAuthorized uemail  =
  throwSError err400 $ UserIsNotAuthrized uemail

throwUserExists :: MonadThrow m => m a
throwUserExists = throwSError err404 UserExistsError

-- | An admin can do anything, an evaluator can do anything for an agent
-- Agent can only access own account
runProtectedAction
  :: (MonadThrow m)
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
               , urEmail = userEmail
               , urRole = userRole
               , urCreatedAt = userCreatedAt
               , urUpdatedAt = userUpdatedAt
               }
