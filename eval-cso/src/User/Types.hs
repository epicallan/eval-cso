module User.Types
        ( Email (..)
        , HasEmail(..), HasRole(..), HasUserName(..), HasUserAttrs, HasPassword(..)
        , HasCreateUserAttrs, HasFullName(..)
        , Login (..)
        , Signup (..)
        , Password (..)
        , PasswordHash (..)
        , Role (..)
        , UserName (..)
        , FullName (..)
        , UserErrors(..)
        , UserEdits(..)
        , UserType (..)
        , UserToken (..)
        , UserResponse (..)
        , UserLoginResponse (..)
        ) where
import Data.Aeson (FromJSON(..), ToJSON(..), genericParseJSON, genericToJSON)
import Data.Aeson.Options as AO (defaultOptions)
import Data.Aeson.TH (Options(..), deriveJSON)
import Data.Char (toLower)
import Data.Kind (Type)
import Data.List (stripPrefix)
import Data.Time (UTCTime)
import Database.Persist.Sql (PersistField)
import Database.Persist.TH (derivePersistField)
import Lens.Micro.Platform (makeFields)
import Web.HttpApiData (FromHttpApiData)

import Common.Types (Id)
import User.Password (Password(..), PasswordHash(..))

data Role =
    CSOAgent
  | Evaluator
  | Supervisor
  | Admin
  deriving (Eq, Ord, Show, Read)

$(deriveJSON AO.defaultOptions ''Role)
derivePersistField "Role"

newtype UserToken = UserToken { unUserToken :: Text }
  deriving (Show)
$(deriveJSON AO.defaultOptions  { unwrapUnaryRecords = True } ''UserToken)

newtype UserName = UserName {unUname :: Text}
  deriving (Eq, Show, PersistField, FromHttpApiData)

$(deriveJSON AO.defaultOptions  { unwrapUnaryRecords = True } ''UserName)

newtype FullName = FullName {unFullName :: Text}
  deriving (Eq, Show, PersistField)

$(deriveJSON AO.defaultOptions  { unwrapUnaryRecords = True } ''FullName)

data UserType a :: Type where
  CSOAgentUser :: Id -> UserType 'CSOAgent
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
  | UserNameNotFound UserName
  | UserExistsError UserName
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
 { _signupUserName :: UserName
 , _signupFullName :: FullName
 , _signupEmail :: Email
 , _signupPassword :: Password
 } deriving (Show)

$(deriveJSON AO.defaultOptions ''Signup)
makeFields ''Signup

userEditsOptions :: Options
userEditsOptions = AO.defaultOptions{ fieldLabelModifier = userEditsLabel }
  where
    userEditsLabel :: String -> String
    userEditsLabel label = maybe label headToLower (stripPrefix "_userEdits" label)

    headToLower :: String -> String
    headToLower []     = error "Can not use headToLower on empty String"
    headToLower (x:xs) = toLower x : xs

data UserEdits = UserEdits
 { _userEditsUserName :: UserName
 , _userEditsEmail :: Email
 , _userEditsFullName :: FullName
 , _userEditsRole :: Role
 } deriving Generic

instance ToJSON UserEdits where
    toJSON = genericToJSON userEditsOptions

instance FromJSON UserEdits where
    parseJSON = genericParseJSON userEditsOptions

makeFields ''UserEdits

data UserResponse = UserResponse
  { urUserName :: UserName
  , urFullName :: FullName
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
  ( HasUserName a UserName
  , HasEmail a Email
  , HasRole a Role
  , HasFullName a FullName
  )

type HasCreateUserAttrs a =
  ( HasUserAttrs a
  , HasPassword a Password
  )

instance HasRole Signup Role where
  role f signup = fmap (const signup) (f CSOAgent)
