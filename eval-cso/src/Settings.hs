{-# LANGUAGE DeriveAnyClass, DerivingStrategies, TypeApplications #-}
module Settings
       ( DbConf (..)
       , HasDbConf (..)
       , HasSettings (..)
       , Port
       , Settings(..)
       , Settings'
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
  { _dbUser     :: Text
  , _dbHost     :: Text
  , _dbName     :: Text
  , _dbPassword :: Text
  } deriving (Show)

$(deriveJSON AO.defaultOptions  ''DbConf)
makeClassy ''DbConf

type family HKD (f :: Type -> Type) (t :: Type) :: Type where
  HKD Identity t = t
  HKD f t = f t

data Settings f = Settings
  { _sAppName :: HKD f Text
  , _sPort    :: HKD f Port
  , _sDbConf  :: HKD f DbConf
  , _sSalt    :: HKD f Text
  }

type Settings' = Settings Identity
type PartialSettings = Settings Last

deriving instance Show Settings'
deriving instance Show PartialSettings

makeClassy ''Settings

newtype SettingsError = SettingsError Text
 deriving Show
 deriving anyclass Exception

instance Semigroup PartialSettings where
  x <> y = Settings
    { _sAppName = _sAppName x <> _sAppName y
    , _sPort    = _sPort x <> _sPort y
    , _sDbConf  = _sDbConf x <> _sDbConf y
    , _sSalt    = _sSalt x <> _sSalt y
    }

instance Monoid PartialSettings where
  mempty = Settings mempty mempty mempty mempty
  mappend = (<>)

instance FromJSON PartialSettings where
  parseJSON = withObject "FromJSON PartialSettings" $ \obj -> do
    _sAppName <- Last <$> obj .:? "appName"
    _sPort <- Last <$> obj .:? "port"
    _sDbConf <- Last <$> obj .:? "dbConf"
    _sSalt <- Last <$>  obj .:? "salt"
    return Settings {..}

lastToEither :: Text -> Last a -> Either SettingsError a
lastToEither errMsg (Last mval) = maybeToRight (SettingsError errMsg) mval

mkSettings :: PartialSettings -> Either SettingsError Settings'
mkSettings Settings{..} = do
  _sAppName <- lastToEither "Missing App Name" _sAppName
  _sPort <- lastToEither "Missing Port" _sPort
  _sDbConf <- lastToEither "Missing DB Config" _sDbConf
  _sSalt <- lastToEither "Missing salt" _sSalt
  return Settings {..}

lookupOption :: forall a m. (Read a, MonadIO m) => Text -> m (Maybe a)
lookupOption env = do
  mvalue <- liftIO $ lookupEnv $ toString env
  pure $ mvalue >>= readMaybe @a

mkEnvSettings :: forall m. MonadIO m => m PartialSettings
mkEnvSettings = do
  _sAppName <- lastTextOption "APP_NAME"
  _sPort <- lastOption "APP_PORT"
  _sSalt <- lastTextOption "APP_SALT"
  pdUser <- lastTextOption "DB_USER"
  pdName <- lastTextOption "DB_NAME"
  pdHost <- lastTextOption "DB_HOST"
  pdPassword <- lastTextOption "DB_PASSWORD"
  let _sDbConf = DbConf <$> pdUser <*> pdHost <*> pdName <*> pdPassword
  return Settings {..}
  where
    lastOption :: forall a. Read a => Text -> m (Last a)
    lastOption env = Last <$> lookupOption @a env

    lastTextOption :: Text -> m (Last Text)
    lastTextOption env = do
      mvalue <- liftIO $ lookupEnv (toString env)
      pure . Last $ toText <$> mvalue

defaultSettings :: PartialSettings
defaultSettings = Settings
  (Last $ Just "eval-app")
  (Last $ Just 8888)
  mempty
  mempty

parseSettings :: (MonadIO m, MonadThrow m) => FilePath -> m Settings'
parseSettings settingsFile = do
  efileSettings <- liftIO $ decodeFileEither @PartialSettings settingsFile
  envSettings <- mkEnvSettings
  let fileSettings = fromRight mempty efileSettings
  let combined = defaultSettings <> fileSettings <> envSettings
  either throwM pure $ mkSettings combined
