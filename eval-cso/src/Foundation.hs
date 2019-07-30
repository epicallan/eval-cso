{-# LANGUAGE InstanceSigs #-}
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
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Logger
  (LogLevel(..), LoggingT, MonadLogger, filterLogger, runStdoutLoggingT)
import Database.Persist.Postgresql (ConnectionString, createPostgresqlPool)
import Database.Persist.Sql (ConnectionPool)
import Lens.Micro.Platform (Lens', makeClassy)
import Servant.Auth.Server (ThrowAll(..))

import Settings
  (DbConf(..), HasDbConf(..), HasSettings(..), Settings(..), lookupOption,
  parseSettings)

-- | Right now, we're distinguishing between three environments
data Environment
    = Development
    | Test
    | Production
    deriving (Eq, Read, Show)

data Config = Config
    { _cPool  :: ConnectionPool
    , _cSettings :: Settings
    , _cEnvironment :: Environment
    }

makeClassy ''Config

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
    _cEnvironment <- lookupSetting "APP_ENV" Development
    _cSettings <- getSettings _cEnvironment
    _cPool  <- makePool _cEnvironment $ _cSettings ^. sDbConf
    pure Config{..}
    where
      getSettings :: Environment -> m Settings
      getSettings = \case
         Development -> parseSettings "./config/dev.yaml"
         Test -> parseSettings "./config/test.yaml"
         Production -> parseSettings "./config/prod.yaml"

lookupSetting :: (MonadUnliftIO m, Read a) => Text -> a -> m a
lookupSetting env def = fromMaybe def <$> lookupOption env

-- | The number of pools to use for a given environment.
getPoolSize :: Environment -> Int
getPoolSize = \ case
    Test        -> 1
    Development -> 1
    Production  -> 8

createConnStr :: DbConf -> ConnectionString
createConnStr dconfig =
     "host="      <> dconfig ^. dbHost
  <> " dbname="   <> dconfig ^. dbName
  <> " user="     <> dconfig ^. dbUser
  <> " password=" <> dconfig ^. dbPassword
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
