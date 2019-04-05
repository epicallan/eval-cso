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
import qualified Data.ByteString.Char8 as BS
import Database.Persist.Postgresql (ConnectionString, createPostgresqlPool)
import Database.Persist.Sql (ConnectionPool)
import Lens.Micro.Platform (Lens', makeClassy, makeLenses)

import Control.Monad.IO.Unlift (MonadUnliftIO)
import Network.Wai.Handler.Warp (Port)
import Servant.Auth.Server (ThrowAll(..))
import System.Environment (lookupEnv)
import System.IO.Error (userError)

-- | Right now, we're distinguishing between three environments
data Environment
    = Development
    | Test
    | Production
    deriving (Eq, Show, Read)


-- | The Config for our application is (for now) the 'Environment' we're
-- running in and a Persistent 'ConnectionPool'.
data Config = Config
    { _cAppName :: Text
    , _cEnvironment     :: Environment
    , _cPort    :: Port
    , _cSalt    :: Text
    }

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
    _cPort  <- lookupSetting "EX_PORT" (pure 8081)
    _cEnvironment   <- lookupSetting "EX_ENV" (pure Development)
    let _cAppName = "App Name"
    let _cSalt = "super-secret"
    pure Config{..}

initEnv :: (MonadThrow m, MonadUnliftIO m) => m Env
initEnv = do
  _eConfig <- acquireConfig
  _ePool  <- makePool (_eConfig ^. cEnvironment)
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

-- | This function creates a 'ConnectionPool' for the given environment.
getConnStr :: (MonadIO m, MonadThrow m) => Environment -> m ConnectionString
getConnStr = \case
    Test        -> pure $ createConnStr "-test"
    _           ->  do
        mConnStr <- liftIO $ runMaybeT $ do
            let envKeys = [ "host="
                          , " dbname="
                          , " user="
                          ]
                envs =    [ "EX_HOST"
                          , "EX_DATABASE"
                          , "EX_USER"
                          ]
            envVars <- traverse (MaybeT . lookupEnv) envs
            pure $ mconcat . zipWith (<>) envKeys $ BS.pack <$> envVars
        putTextLn $ " connection : " <> show mConnStr -- TODO: use proper logger
        case mConnStr of
            -- If we don't have a correct database configuration, we can't
            Nothing  -> throwM (userError "Database Configuration not present in environment.")
            Just str -> pure str

-- | The number of pools to use for a given environment.
getPoolSize :: Environment -> Int
getPoolSize = \ case
    Test        -> 1
    Development -> 1
    Production  -> 8

-- | A basic 'ConnectionString' for local/test development. Pass in either
-- @""@ for 'Development' or @"test"@ for 'Test'.
createConnStr :: BS.ByteString -> ConnectionString
createConnStr sfx = "host=localhost dbname=app" <> sfx <> " user=test"

filterLogsByLevel :: Environment -> LogLevel -> Bool
filterLogsByLevel = \case
  Production -> (>= LevelWarn)
  _          -> (>= LevelDebug)

filterLogs :: Environment -> LoggingT m a -> LoggingT m a
filterLogs env = filterLogger (\_ lv -> filterLogsByLevel env lv)

-- | This function creates a 'ConnectionPool' for the given environment.
makePool :: (MonadThrow m , MonadUnliftIO m) => Environment -> m ConnectionPool
makePool env = do
    let poolSize = getPoolSize env
    connStr      <- getConnStr env
    runStdoutLoggingT $ filterLogs env $ createPostgresqlPool connStr poolSize
