module User.Types
        ( Email (..)
        , HasEmail, HasRole, HasName, HasUserAttrs, HasPassword
        , HasCreateUserAttrs, HasFullName
        , Login (..)
        , Signup (..)
        , Password (..)
        , PasswordHash (..)
        , Role (..)
        , Uname (..)
        , UFullName (..)
        , UserErrors(..)
        , UserEdits(..)
        , UserType (..)
        , UserToken (..)
        , UserResponse (..)
        , UserLoginResponse (..)
        , role, name, email, password, fullName
        ) where

import Data.Aeson.Options as AO (defaultOptions)
import Data.Aeson.TH (Options(..), deriveJSON)
import Data.Kind (Type)
import Data.Time (UTCTime)
import Database.Persist.Sql (PersistField)
import Database.Persist.TH (derivePersistField)
import Lens.Micro.Platform (makeFields)

import Common.Types (Id)
import User.Password (Password(..), PasswordHash(..))

data Role =
    Admin
  | CsoAgent
  | Evaluator
  | Supervisor
  deriving (Eq, Show, Read)

$(deriveJSON AO.defaultOptions ''Role)
derivePersistField "Role"

newtype UserToken = UserToken { unUserToken :: Text }
  deriving (Show)
$(deriveJSON AO.defaultOptions  { unwrapUnaryRecords = True } ''UserToken)

newtype Uname = Uname {unUname :: Text}
  deriving (Eq, Show, PersistField)

$(deriveJSON AO.defaultOptions  { unwrapUnaryRecords = True } ''Uname)

newtype UFullName = UFullName {unUFullName :: Text}
  deriving (Eq, Show, PersistField)

$(deriveJSON AO.defaultOptions  { unwrapUnaryRecords = True } ''UFullName)

data UserType a :: Type where
  CsoAgentUser :: Id -> UserType 'CsoAgent
  AdminUser :: Id -> UserType 'Admin
  EvaluatorUser :: Id -> UserType 'Evaluator

newtype Email = Email {unEmail :: Text}
  deriving (Eq, Show, PersistField)

$(deriveJSON AO.defaultOptions { unwrapUnaryRecords = True } ''Email)

data UserErrors =
    UserEmailNotFound Email
  | IncorrectPassword Email
  | CookieSetupError Text
  | UserIsNotAuthrized Email
  | UserNameNotFound Uname
  | UserExistsError Uname
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
 { _signupName :: Uname
 , _signupFullName :: UFullName
 , _signupEmail :: Email
 , _signupPassword :: Password
 } deriving (Show)

$(deriveJSON AO.defaultOptions ''Signup)
makeFields ''Signup

data UserEdits = UserEdits
 { _userEditsName :: Uname
 , _userEditsEmail :: Email
 , _userEditsFullName :: UFullName
 , _userEditsRole :: Role
 }

$(deriveJSON AO.defaultOptions ''UserEdits)
makeFields ''UserEdits

data UserResponse = UserResponse
  { urName :: Uname
  , urFullName :: UFullName
  , urEmail :: Email
  , urRole :: Role
  , urCreatedAt :: UTCTime
  , urUpdatedAt :: UTCTime
  } deriving Show

$(deriveJSON AO.defaultOptions ''UserResponse)

data UserLoginResponse = UserLoginResponse
  { ulToken :: UserToken
  , ulUser :: UserResponse
  } deriving Show

$(deriveJSON AO.defaultOptions ''UserLoginResponse)


type HasUserAttrs a =
  ( HasName a Uname
  , HasEmail a Email
  , HasRole a Role
  , HasFullName a UFullName
  )

type HasCreateUserAttrs a =
  ( HasUserAttrs a
  , HasPassword a Password
  )

instance HasRole Signup Role where
  role f signup = fmap (const signup) (f CsoAgent)
