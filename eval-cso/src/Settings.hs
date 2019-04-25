module Settings
       ( DbConf (..)
       , HasDbConf (..)
       , HasSettings (..)
       , Port
       , Settings(..)
       , lookupOption
       , parseSettings
       ) where
import Data.Aeson (FromJSON(..), withObject, (.:?))
import Data.Monoid (Last(..))
import Data.Yaml (decodeFileThrow)
import Lens.Micro.Platform (makeClassy)
import System.Environment (lookupEnv)

type Port = Natural

data DbConf = DbConf
  { _dbUser :: Text
  , _dbHost :: Text
  , _dbName :: Text
  , _dbPassword :: Text
  } deriving (Show)

makeClassy ''DbConf

data Settings = Settings
  { _sAppName :: Text
  , _sPort    :: Port
  , _sDbConf  :: DbConf
  , _sSalt    :: Text
  } deriving (Show)
makeClassy ''Settings

data PartialSettings = PartialSettings
  { psAppName :: Last Text
  , psPort    :: Last Port
  , psDbConf  :: Last PartialDbConf
  , psSalt    :: Last Text
  } deriving Show

data PartialDbConf = PartialDbConf
  { pdUser     :: Last Text
  , pdHost     :: Last Text
  , pdName     :: Last Text
  , pdPassword :: Last Text
  } deriving Show

newtype SettingsError = SettingsError Text
 deriving Show

instance Exception SettingsError

instance Semigroup PartialDbConf where
  x <> y = PartialDbConf
    { pdUser = pdUser x <> pdUser y
    , pdHost = pdHost x <> pdHost y
    , pdName = pdName x <> pdName y
    , pdPassword = pdPassword x <> pdPassword y
    }

instance Monoid PartialDbConf where
  mempty = PartialDbConf mempty mempty mempty mempty
  mappend = (<>)

instance Semigroup PartialSettings where
  x <> y = PartialSettings
    { psAppName = psAppName x <> psAppName y
    , psPort    = psPort x <> psPort y
    , psDbConf  = psDbConf x <> psDbConf y
    , psSalt    = psSalt x <> psSalt y
    }

instance Monoid PartialSettings where
  mempty = PartialSettings mempty mempty mempty mempty
  mappend = (<>)

instance FromJSON PartialDbConf where
  parseJSON = withObject "FromJSON PartialDbConf" $ \obj -> do
    pdUser <- Last <$> obj .:? "user"
    pdHost <- Last <$> obj .:? "host"
    pdName <- Last <$> obj .:? "name"
    pdPassword <- Last <$>  obj .:? "password"
    return PartialDbConf {..}

instance FromJSON PartialSettings where
  parseJSON = withObject "FromJSON PartialSettings" $ \obj -> do
    psAppName <- Last <$> obj .:? "appName"
    psPort <- Last <$> obj .:? "port"
    psDbConf <- Last <$> obj .:? "dbConf"
    psSalt <- Last <$>  obj .:? "salt"
    return PartialSettings {..}

lastToEither :: Text -> Last a -> Either SettingsError a
lastToEither errMsg (Last mval) = maybeToRight (SettingsError errMsg) mval

mkSettings :: PartialSettings -> Either SettingsError Settings
mkSettings PartialSettings{..} = do
  _sAppName <- lastToEither "Missing App Name" psAppName
  _sPort <- lastToEither "Missing Port" psPort
  pDbConf <- lastToEither "Missing DB Config" psDbConf
  _sSalt <- lastToEither "Missing salt" psSalt
  _sDbConf <- mkDbConf pDbConf
  return Settings {..}

mkDbConf :: PartialDbConf -> Either SettingsError DbConf
mkDbConf PartialDbConf{..} = do
  _dbUser <- lastToEither "Missing DB user" pdUser
  _dbHost <- lastToEither "Missing DB host" pdHost
  _dbName <- lastToEither "Missing DB name" pdName
  _dbPassword <- lastToEither "Missing DB password" pdPassword
  return DbConf {..}

lookupOption :: (Read a, MonadIO m) => Text -> m (Maybe a)
lookupOption env = do
  mvalue <- liftIO $ lookupEnv $ toString env
  pure $ mvalue >>= readMaybe

mkEnvSettings :: MonadIO m => m PartialSettings
mkEnvSettings = do
  psAppName <- lastOption "APP_NAME"
  psPort <- lastOption "APP_PORT"
  psSalt <- lastOption "APP_SALT"
  pdUser <- lastOption "DB_USER"
  pdName <- lastOption "DB_NAME"
  pdHost <- lastOption "DB_HOST"
  pdPassword <- lastOption "DB_PASSWORD"
  let psDbConf = PartialDbConf <$> pdUser <*> pdName <*> pdHost <*> pdPassword
  return PartialSettings {..}
  where
    lastOption env = Last <$> lookupOption env

defaultSettings :: PartialSettings
defaultSettings = PartialSettings
  (Last $ Just "eval-app")
  (Last $ Just 8888)
  mempty
  mempty

parseSettings :: (MonadIO m, MonadThrow m) => FilePath -> m Settings
parseSettings settingsFile = do
  fileSettings <- decodeFileThrow settingsFile
  envSettings <- mkEnvSettings
  let combined = defaultSettings <> fileSettings <> envSettings
  either throwM pure $ mkSettings combined
