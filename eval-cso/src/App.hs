module App (runApp) where

import Data.Pool (destroyAllResources)
import Network.HTTP.Types.Method (methodDelete, methodPatch, methodPut)
import Network.Wai (Middleware)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors
  (CorsResourcePolicy(..), cors, simpleCorsResourcePolicy, simpleMethods)
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
  app conf >>= run (fromIntegral $ conf ^. sPort) . middleware . appCors
  where
    middleware :: Middleware
    middleware = case conf ^. cEnvironment of
      Production -> logStdout . gzip def
      _          -> logStdoutDev

    appCors :: Middleware
    appCors = cors (const $ Just policy)

    policy :: CorsResourcePolicy
    policy = simpleCorsResourcePolicy
                { corsMethods =  simpleMethods
                              <> [methodPatch, methodPut, methodDelete]
                , corsRequestHeaders = ["Authorization", "Content-Type"]
                }

shutdownApp :: Config -> IO ()
shutdownApp = destroyAllResources . view  pool
