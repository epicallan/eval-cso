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
import User.Model.Types (LoggedInUser, SafeUser(..))
import User.Types (Email, Role(..), UserErrors(..), UserName, UserResponse(..))

throwInvalidUserName :: MonadThrowLogger m => UserName -> m a
throwInvalidUserName = throwSError err404 . UserNameNotFound

throwUserNotAuthorized :: MonadThrowLogger m => Email -> m a
throwUserNotAuthorized uemail  =
  throwSError err400 $ UserIsNotAuthrized uemail

throwUserExists :: MonadThrowLogger m => UserName ->  m a
throwUserExists = throwSError err400 . UserExistsError

runEvaluatorAction :: MonadThrowLogger m => LoggedInUser -> m a -> m a
runEvaluatorAction (unSafeUser -> user) action = case userRole user of
  Evaluator -> action
  Admin     -> action
  _         -> throwUserNotAuthorized $ userEmail user

runProtectedAction
  :: MonadThrowLogger m
  => LoggedInUser
  -> Role -- ^ role for user who consumes the action
  -> UserName -- ^ name for user who consumes the action
  -> m a -- ^ action to run
  -> m a
runProtectedAction (unSafeUser -> logedInUser) consumerRole consumerUserName action =
  case userRole logedInUser of
    CSOAgent | userName logedInUser == consumerUserName -> action
    _        -> if consumerRole <= userRole logedInUser
                    then action
                    else throwUserNotAuthorized $ userEmail logedInUser

runAdminAction
  :: MonadThrowLogger m
  => LoggedInUser
  -> m a -- ^ action to run
  -> m a
runAdminAction (unSafeUser -> user) action =
  case userRole user of
    Admin -> action
    _     -> throwUserNotAuthorized $ userEmail user

toUserResponse :: User -> UserResponse
toUserResponse User{..} =
  UserResponse { urUserName = userName
               , urFullName = userFullName
               , urEmail = userEmail
               , urRole = userRole
               , urCreatedAt = userCreatedAt
               , urUpdatedAt = userUpdatedAt
               }
