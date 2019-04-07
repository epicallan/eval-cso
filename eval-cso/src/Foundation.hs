module Foundation
       ( initEnv
       , filterLogs
       , Config (..)
       , AppT (..)
       , App
       , Settings(..)
       , Environment (..)
       , HasEnvironment (..)
       , HasPool (..)
       , HasConfig(..)
       , HasSettings (..)
       ) where
import Control.Monad.Logger
  (LogLevel(..), LoggingT, MonadLogger, filterLogger, runStdoutLoggingT)
import Data.Aeson.Options as AO (defaultOptions)
import Data.Aeson.TH (deriveJSON)
import Data.Yaml (decodeFileThrow)
import Database.Persist.Postgresql (ConnectionString, createPostgresqlPool)
import Database.Persist.Sql (ConnectionPool)
import Lens.Micro.Platform (Lens', makeClassy)

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

data Settings = Settings
    { _sAppName :: Text
    , _sPort    :: Port
    , _sDbConf :: DbConf
    , _sSalt    :: Text
    } deriving (Show)

$(deriveJSON AO.defaultOptions ''Settings)

data Config = Config
    { _cPool  :: ConnectionPool
    , _cSettings :: Settings
    , _cEnvironment :: Environment
    }

makeClassy ''Config
makeClassy ''Settings

class HasPool a where
  pool :: Lens' a ConnectionPool

instance HasPool Config where
  pool = cPool

class HasEnvironment a where
  environment :: Lens' a Environment

instance HasEnvironment Config where
  environment = cEnvironment

instance HasSettings Config where
  settings = cSettings

newtype AppT m a = AppT { runApp :: ReaderT Config m a }
    deriving ( Functor, Applicative, Monad, MonadIO
             , MonadReader Config, MonadThrow, MonadCatch
             , MonadTrans, MonadLogger
             )

type App = AppT (LoggingT IO)

instance MonadThrow m => ThrowAll (AppT m a) where
  throwAll = throwM

initEnv :: forall m. (MonadUnliftIO m, MonadThrow m) => m Config
initEnv = do
    _cEnvironment <- lookupSetting "ENV" (pure Development)
    _cSettings <- getSettings _cEnvironment
    _cPool  <- makePool _cEnvironment $ _cSettings ^. sDbConf
    pure Config{..}
    where
      getSettings :: Environment -> m Settings
      getSettings = \case
         Production -> decodeFileThrow "./config/prod.yaml"
         Development -> decodeFileThrow "./config/dev.yaml"
         Test -> decodeFileThrow "./config.test.yaml"

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
