module App (runApp) where

import Data.Pool (destroyAllResources)
import Network.Wai (Middleware)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Gzip (def, gzip)
import Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)

import Api (app)
import Foundation (Config, Environment(..), HasConfig(..), initEnv, pool, sPort)

runApp :: IO ()
runApp = bracket initEnv shutdownApp startApp

startApp :: Config -> IO ()
startApp conf = do
  app conf >>= run (fromIntegral $ conf ^. sPort) . middleware
  putTextLn $ "running server on port: " <> show (conf ^. sPort)
  where
    middleware :: Middleware
    middleware = case conf ^. cEnvironment of
      Production -> logStdout . gzip def
      _          -> logStdoutDev

shutdownApp :: Config -> IO ()
shutdownApp = destroyAllResources . view  pool
