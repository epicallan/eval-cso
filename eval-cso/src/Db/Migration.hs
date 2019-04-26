module Db.Migration
        ( runSeeder
        , runDbMigrations
        ) where

import Control.Monad.Logger (runStderrLoggingT)
import Control.Monad.Time (MonadTime)
import Data.Aeson (eitherDecodeFileStrict)
import Data.Aeson.Options as AO (defaultOptions)
import Data.Aeson.TH (deriveJSON)
import Data.Pool (destroyAllResources)
import Lens.Micro.Platform (makeLenses)

import Agent.Model.Internal (agentModel)
import Agent.Model.Types (AgentModel(amCreateBranch))
import Agent.Types (Bname)
import Common.Errors (MonadThrowLogger)
import Db.Model (CanDb, runMigrations)
import Evaluation.Model.Internal (evalModel)
import Evaluation.Model.Types (EvalModel(emCreateService))
import Evaluation.Types (ServiceAttrs)
import Foundation (Config, HasPool(..), HasSettings, initEnv)
import User.Controller (signupUser)
import User.Model.Internal (userModel)
import User.Types
  (Email, HasEmail(..), HasFullName(..), HasName(..), HasPassword(..),
  HasRole(..), Password, Role(..), Signup, UFullName, Uname)

newtype ReadSeedFileError = ReadSeedFileError Text
  deriving (Show)

instance Exception ReadSeedFileError

type CanMigrate m r = (CanDb m r, MonadThrowLogger m, MonadTime m)

newtype AdminUser = AdminUser { unAdminUser :: Signup }

-- | we don't need setters
instance HasName AdminUser Uname where
  name f admin = fmap (const admin)
                      (f $ unAdminUser admin ^. name)

instance HasFullName AdminUser UFullName where
  fullName f admin = fmap (const admin)
                      (f $ unAdminUser admin ^. fullName)

instance HasRole AdminUser Role where
  role f admin = fmap (const admin)
                      (f Admin)

instance HasEmail AdminUser Email where
  email f admin = fmap (const admin)
                       (f $ unAdminUser admin ^. email)

instance HasPassword AdminUser Password where
  password f admin = fmap (const admin)
                          (f $ unAdminUser admin ^. password)

data SeedData = SeedData
  { _sdUser :: Signup
  , _sdBranches :: [Bname]
  , _sdServices :: [ServiceAttrs]
  } deriving (Show)

makeLenses ''SeedData
$(deriveJSON AO.defaultOptions ''SeedData)

readSeedJson :: (MonadIO m, MonadThrow m) => m SeedData
readSeedJson = do
  eSeedData <- liftIO $ eitherDecodeFileStrict "config/seed.json"
  either (throwM . ReadSeedFileError . toText ) pure eSeedData

mkBranches :: CanMigrate m r => [Bname] -> m ()
mkBranches = mapM_ (amCreateBranch agentModel)

mkAdminUser :: (CanMigrate m r, HasSettings r) => AdminUser -> m ()
mkAdminUser us = signupUser userModel us >> pass

mkServices :: CanMigrate m r => [ServiceAttrs] -> m ()
mkServices = mapM_ (emCreateService evalModel)

startSeeder :: Config -> IO ()
startSeeder conf = do
  seedData <- readSeedJson
  runStderrLoggingT $ usingReaderT conf $ do
    mkAdminUser $ AdminUser $ seedData ^. sdUser
    mkBranches $ seedData ^. sdBranches
    mkServices $ seedData ^. sdServices

shutdownMigration :: Config -> IO ()
shutdownMigration conf = do
  putTextLn "shutting down Migration.."
  destroyAllResources $ conf ^. pool

startMigration :: Config -> IO ()
startMigration = runStderrLoggingT . runReaderT runMigrations

runSeeder:: IO ()
runSeeder = do
  putTextLn "start seeder..."
  bracket initEnv shutdownMigration startSeeder

runDbMigrations :: IO ()
runDbMigrations = do
  putTextLn "start Migration.."
  bracket initEnv shutdownMigration startMigration
