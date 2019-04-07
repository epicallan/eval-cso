module App (runApp) where

import Data.Pool (destroyAllResources)
import Network.Wai (Middleware)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Gzip (def, gzip)
import Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)

import Api (app)
import Foundation
  (Config, Environment(..), HasConfig(..), HasSettings(..), initEnv, pool)
import Db.Model (runMigrations)

runApp :: IO ()
runApp = bracket initEnv shutdownApp startApp

startApp :: Config -> IO ()
startApp conf = do
  usingReaderT conf runMigrations
  putTextLn $ "starting " <> conf ^. sAppName <> " on port " <> show (conf ^. sPort)
  app conf >>= run (fromIntegral $ conf ^. sPort) . middleware
  where
    middleware :: Middleware
    middleware = case conf ^. cEnvironment of
      Production -> logStdout . gzip def
      _          -> logStdoutDev

shutdownApp :: Config -> IO ()
shutdownApp = destroyAllResources . view  pool
