module User.Helper
       ( runProtectedAction
       , runAdminAction
       , runEvaluatorAction
       , toUserResponse
       , throwInvalidUserName
       , throwUserExists
       ) where
import Servant (err400, err404)

import Common.Errors (MonadThrowLogger, throwSError)
import Db.Model (User(..))
import User.Types (Email, Role(..), UserErrors(..), UserName, UserResponse(..))

throwInvalidUserName :: MonadThrowLogger m => UserName -> m a
throwInvalidUserName = throwSError err404 . UserNameNotFound

throwUserNotAuthorized :: MonadThrowLogger m => Email -> m a
throwUserNotAuthorized uemail  =
  throwSError err400 $ UserIsNotAuthrized uemail

throwUserExists :: MonadThrowLogger m => UserName ->  m a
throwUserExists = throwSError err400 . UserExistsError

runEvaluatorAction :: MonadThrowLogger m => User -> m a -> m a
runEvaluatorAction User{..} action = case userRole of
  Evaluator -> action
  Admin     -> action
  _         -> throwUserNotAuthorized userEmail

runProtectedAction
  :: MonadThrowLogger m
  => User -- ^ current logged in user
  -> Role -- ^ role for user who consumes the action
  -> UserName -- ^ name for user who consumes the action
  -> m a -- ^ action to run
  -> m a
runProtectedAction logedInUser consumerRole consumerUserName action =
  case userRole logedInUser of
    CSOAgent | userName logedInUser == consumerUserName -> action
    _        -> if consumerRole <= userRole logedInUser
                    then action
                    else throwUserNotAuthorized uemail
   where
      uemail = userEmail logedInUser

runAdminAction
  :: MonadThrowLogger m
  => User -- ^ current logged in user
  -> m a -- ^ action to run
  -> m a
runAdminAction User{..} action =
  case userRole of
    Admin -> action
    _     -> throwUserNotAuthorized userEmail

toUserResponse :: User -> UserResponse
toUserResponse User{..} =
  UserResponse { urUserName = userName
               , urFullName = userFullName
               , urEmail = userEmail
               , urRole = userRole
               , urCreatedAt = userCreatedAt
               , urUpdatedAt = userUpdatedAt
               }
