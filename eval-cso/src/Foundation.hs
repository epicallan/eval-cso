module Foundation
       ( initEnv
       , filterLogs
       , Config (..)
       , AppT (..)
       , App
       , Env (..)
       , Environment (..)
       , HasPool (..)
       , HasConfig (..)
       ) where
import Control.Monad.Logger
  (LogLevel(..), LoggingT, MonadLogger, filterLogger, runStdoutLoggingT)
import Data.Aeson.Options as AO (defaultOptions)
import Data.Aeson.TH (deriveJSON)
import Data.Yaml (decodeFileThrow)
import Database.Persist.Postgresql (ConnectionString, createPostgresqlPool)
import Database.Persist.Sql (ConnectionPool)
import Lens.Micro.Platform (Lens', makeClassy, makeLenses)

import Control.Monad.IO.Unlift (MonadUnliftIO)
import Servant.Auth.Server (ThrowAll(..))
import System.Environment (lookupEnv)
import System.IO.Error (userError)

type Port = Natural

-- | Right now, we're distinguishing between three environments
data Environment
    = Development
    | Test
    | Production
    deriving (Eq, Read, Show)

$(deriveJSON AO.defaultOptions ''Environment)

data DbConf = DbConf
  { _dbUser :: Text
  , _dbHost :: Text
  , _dbName :: Text
  } deriving (Show)

makeClassy ''DbConf

$(deriveJSON AO.defaultOptions ''DbConf)

-- | The Config for our application is (for now) the 'Environment' we're
-- running in and a Persistent 'ConnectionPool'.
data Config = Config
    { _cAppName :: Text
    , _cEnvironment :: Environment
    , _cPort    :: Port
    , _cDbConf :: DbConf
    , _cSalt    :: Text
    } deriving (Show)

$(deriveJSON AO.defaultOptions ''Config)

data Env = Env
    { _ePool  :: ConnectionPool
    , _eConfig :: Config
    }

makeLenses ''Env
makeClassy ''Config

instance HasConfig Env where
  config = eConfig

class HasPool a where
  pool :: Lens' a ConnectionPool

instance HasPool Env where
  pool = ePool

newtype AppT m a = AppT { runApp :: ReaderT Env m a }
    deriving ( Functor, Applicative, Monad, MonadIO
             , MonadReader Env, MonadThrow, MonadCatch
             , MonadTrans, MonadLogger
             )

type App = AppT (LoggingT IO)

instance MonadThrow m => ThrowAll (AppT m a) where
  throwAll = throwM

acquireConfig :: (MonadUnliftIO m, MonadThrow m) => m Config
acquireConfig = do
    environment <- lookupSetting "ENV" (pure Development)
    case environment of
      Production -> decodeFileThrow "./config/prod.yaml"
      Development -> decodeFileThrow "./config/dev.yaml"
      Test -> decodeFileThrow "./config.test.yaml"

initEnv :: (MonadThrow m, MonadUnliftIO m) => m Env
initEnv = do
  _eConfig <- acquireConfig
  _ePool  <- makePool (_eConfig ^. cEnvironment) $ _eConfig ^. cDbConf
  pure Env{..}

-- | Looks up a setting in the environment, with a provided default, and
-- 'read's that information into the inferred type.
lookupSetting :: (MonadThrow m, MonadUnliftIO m, Read a) => String -> m a -> m a
lookupSetting env def = do
    maybeValue <- liftIO $ lookupEnv env
    case maybeValue of
        Nothing  -> def
        Just str -> maybe (handleFailedRead str env) return (readMaybe str)

handleFailedRead :: (Show a, MonadThrow m) => a -> String -> m b
handleFailedRead str env =
    throwM $ userError $ mconcat
        [ "Failed to read env \""
        , show str
        , "\" for environment variable "
        , env
        ]

-- | The number of pools to use for a given environment.
getPoolSize :: Environment -> Int
getPoolSize = \ case
    Test        -> 1
    Development -> 1
    Production  -> 8

createConnStr :: DbConf -> ConnectionString
createConnStr dconfig =
     "host="    <> dconfig ^. dbHost
  <> " dbname=" <> dconfig ^. dbName
  <> " user="   <> dconfig ^. dbUser
  & encodeUtf8

filterLogsByLevel :: Environment -> LogLevel -> Bool
filterLogsByLevel = \case
  Development -> (>= LevelDebug)
  Test        -> (>= LevelError)
  Production  -> (>= LevelWarn)

filterLogs :: Environment -> LoggingT m a -> LoggingT m a
filterLogs env = filterLogger (\_ lv -> filterLogsByLevel env lv)

-- | This function creates a 'ConnectionPool' for the given environment.
makePool :: MonadUnliftIO m => Environment -> DbConf -> m ConnectionPool
makePool env dconfig =
    let poolSize = getPoolSize env
        connStr  = createConnStr dconfig
    in runStdoutLoggingT $ filterLogs env $ createPostgresqlPool connStr poolSize
