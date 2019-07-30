{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveAnyClass, DerivingStrategies #-}
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
import Data.Aeson.Options as AO (defaultOptions)
import Data.Aeson.TH (deriveJSON)
import Data.Monoid (Last(..))
import Data.Text ()
import Data.Yaml (decodeFileEither)
import Lens.Micro.Platform (makeClassy)
import System.Environment (lookupEnv)

type Port = Natural

data DbConf = DbConf
  { _dbUser :: Text
  , _dbHost :: Text
  , _dbName :: Text
  , _dbPassword :: Text
  } deriving (Show)

$(deriveJSON AO.defaultOptions  ''DbConf)
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
  , psDbConf  :: Last DbConf
  , psSalt    :: Last Text
  } deriving (Show)

newtype SettingsError = SettingsError Text
 deriving Show
 deriving anyclass Exception

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
  _sDbConf <- lastToEither "Missing DB Config" psDbConf
  _sSalt <- lastToEither "Missing salt" psSalt
  return Settings {..}

lookupOption :: forall a m. (Read a, MonadIO m) => Text -> m (Maybe a)
lookupOption env = do
  mvalue <- liftIO $ lookupEnv $ toString env
  pure $ mvalue >>= readMaybe @a

mkEnvSettings :: forall m. MonadIO m => m PartialSettings
mkEnvSettings = do
  psAppName <- lastTextOption "APP_NAME"
  psPort <- lastOption "APP_PORT"
  psSalt <- lastTextOption "APP_SALT"
  pdUser <- lastTextOption "DB_USER"
  pdName <- lastTextOption "DB_NAME"
  pdHost <- lastTextOption "DB_HOST"
  pdPassword <- lastTextOption "DB_PASSWORD"
  let psDbConf = DbConf <$> pdUser <*> pdHost <*> pdName <*> pdPassword
  return PartialSettings {..}
  where
    lastOption :: forall a. Read a => Text -> m (Last a)
    lastOption env = Last <$> lookupOption @a env

    lastTextOption :: Text -> m (Last Text)
    lastTextOption env = do
      mvalue <- liftIO $ lookupEnv (toString env)
      pure . Last $ toText <$> mvalue

defaultSettings :: PartialSettings
defaultSettings = PartialSettings
  (Last $ Just "eval-app")
  (Last $ Just 8888)
  mempty
  mempty

parseSettings :: (MonadIO m, MonadThrow m) => FilePath -> m Settings
parseSettings settingsFile = do
  efileSettings <- liftIO $ decodeFileEither @PartialSettings settingsFile
  envSettings <- mkEnvSettings
  let fileSettings = fromRight mempty efileSettings
  let combined = defaultSettings <> fileSettings <> envSettings
  either throwM pure $ mkSettings combined

