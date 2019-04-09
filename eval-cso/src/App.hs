module App (runApp) where

import Data.Pool (destroyAllResources)
import Network.Wai (Middleware)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors (simpleCors)
import Network.Wai.Middleware.Gzip (def, gzip)
import Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)
import System.Remote.Monitoring (forkServer)

import Api (app)
import Foundation
  (Config, Environment(..), HasConfig(..), HasSettings(..), initEnv, pool)

runApp :: IO ()
runApp = bracket initEnv shutdownApp startApp

startApp :: Config -> IO ()
startApp conf = do
  forkServer "127.0.0.1" 8080
  putTextLn $ "starting " <> conf ^. sAppName <> " on port " <> show (conf ^. sPort)
  app conf >>= run (fromIntegral $ conf ^. sPort) . middleware
  where
    middleware :: Middleware
    middleware = case conf ^. cEnvironment of
      Production -> logStdout . gzip def . simpleCors
      _          -> logStdoutDev

shutdownApp :: Config -> IO ()
shutdownApp = destroyAllResources . view  pool
