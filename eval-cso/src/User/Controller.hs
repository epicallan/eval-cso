module User.Controller
       ( getUserByEmail
       , createUser
       , listUsers
       ) where

import Servant (err400)

import Common.Types (Id)
import Common.Errors (eitherSError)
import Model (User)
import User.Storage.Types (UserStorage(..))
import User.Types (Email(..))

getUserByEmail
  :: forall m .(MonadThrow m)
  => UserStorage m
  -> Text
  -> m User
getUserByEmail us email = do
  eUser <- usGetUserByEmail us $ Email email
  eitherSError err400 eUser

createUser
  :: UserStorage m
  -> User
  -> m Id
createUser = usCreateUser

listUsers
  :: UserStorage m
  -> m [User]
listUsers = usAllUsers

