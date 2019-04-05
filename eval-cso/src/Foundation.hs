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
       , Port (..)
       ) where
import Control.Monad.Logger
  (LogLevel(..), LoggingT, MonadLogger, filterLogger, runStdoutLoggingT)
import Database.Persist.Postgresql (ConnectionString, createPostgresqlPool)
import Database.Persist.Sql (ConnectionPool)
import Dhall (Interpret, auto, detailed, input)
import Lens.Micro.Platform (Lens', makeClassy, makeLenses)

import Control.Monad.IO.Unlift (MonadUnliftIO)
import Servant.Auth.Server (ThrowAll(..))
import System.Environment (lookupEnv)
import System.IO.Error (userError)

newtype Port = Port { unPort :: Natural }
  deriving (Eq, Show, Generic)

instance Interpret Port

-- | Right now, we're distinguishing between three environments
data Environment
    = Development
    | Test
    | Production
    deriving (Eq, Show, Read, Generic)

instance Interpret Environment

data DbConf = DbConf
  { _dbUser :: Text
  , _dbHost :: Text
  , _dbName :: Text
  } deriving (Show, Generic)

makeClassy ''DbConf

instance Interpret DbConf

-- | The Config for our application is (for now) the 'Environment' we're
-- running in and a Persistent 'ConnectionPool'.
data Config = Config
    { _cAppName :: Text
    , _cEnvironment :: Environment
    , _cPort    :: Port
    , _cDbConf :: DbConf
    , _cSalt    :: Text
    } deriving (Show, Generic)

instance Interpret Config

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

-- TODO: get this all config from a secrets config file
acquireConfig :: (MonadUnliftIO m, MonadThrow m) => m Config
acquireConfig = do
    environment <- lookupSetting "ENV" (pure Development)
    liftIO $ detailed $ case environment of
      Development -> input auto "./config/dev.dhall"
      Production -> input auto "./config/prod.dhall"
      Test -> input auto "./config/test.dhall"

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
        [ "Failed to read \""
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
