module User.Types
        ( Email (..)
        , HasEmail, HasRole, HasUserAttrs, HasPassword
        , Login (..)
        , Signup (..)
        , Password (..)
        , PasswordHash (..)
        , Role (..)
        , ServantAuthHeaders
        , UserErrors(..)
        , Edits(..)
        , UserResponse (..)
        , role, name, email, password
        ) where

import Data.Aeson.TH (Options(..), deriveJSON)
import Data.Aeson.Options as AO (defaultOptions)
import Database.Persist.Sql (PersistField)
import Database.Persist.TH (derivePersistField)
import Data.Time (UTCTime)
import Lens.Micro.Platform (makeFields)

import Servant
import Servant.Auth.Server

import User.Password (Password(..), PasswordHash(..))
import Common.Types (Name, Id)

type ServantAuthHeaders = Headers '[ Header "Set-Cookie" SetCookie
                                   , Header "Set-Cookie" SetCookie
                                   ] NoContent

data Role =
    Admin
  | Member
  | Evaluator
  | Supervisor
  deriving (Eq, Show, Read)

$(deriveJSON AO.defaultOptions ''Role)
derivePersistField "Role"

newtype Email = Email {unEmail :: Text}
  deriving (Eq, Show, PersistField)

$(deriveJSON AO.defaultOptions { unwrapUnaryRecords = True } ''Email)

data UserErrors =
    UserEmailNotFound Email
  | IncorrectPassword Email
  | CookieSetupError Email
  | UserIsNotAuthrized Email
  | UserNameNotFound Name
  | UserNotFound Id
  deriving Show

instance Exception UserErrors

newtype PasswordPlain = PasswordPlain {unPasswordPlain :: Text}
  deriving (Eq, Show, PersistField)

data Login = Login
  { _loginEmail :: Email
  , _loginPassword :: Password
  }

makeFields ''Login
$(deriveJSON AO.defaultOptions ''Login)

data Signup = Signup
 { _signupName :: Name
 , _signupEmail :: Email
 , _signupPassword :: Password
 }

$(deriveJSON AO.defaultOptions ''Signup)
makeFields ''Signup

data Edits = Edits
 { _editsName :: Name
 , _editsEmail :: Email
 , _editsRole :: Role
 }

$(deriveJSON AO.defaultOptions ''Edits)
makeFields ''Edits

data UserResponse = UserResponse
  { urName :: Name
  , urEmail :: Email
  , urRole :: Role
  , urCreatedAt :: UTCTime
  , urUpdatedAt :: UTCTime
  } deriving Show

$(deriveJSON AO.defaultOptions ''UserResponse)

type HasUserAttrs a =
  ( HasName a Name
  , HasEmail a Email
  , HasRole a Role
  )

instance HasRole Signup Role where
  role f signup = fmap (const signup) (f Member)

