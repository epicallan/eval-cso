module App (runApp) where

import Data.Pool (destroyAllResources)
import Network.Wai (Middleware)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Gzip (def, gzip)
import Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)

import Api (app)
import Foundation
  (Env, Environment(..), HasConfig(..), HasPool(..), Port(..), initEnv)

runApp :: IO ()
runApp = bracket initEnv shutdownApp startApp

startApp :: Env -> IO ()
startApp env = do
  app env >>= run (fromIntegral $ unPort $ env ^. cPort) . middleware
  putTextLn $ "running server on port: " <> show (env ^. cPort)
  where
    middleware :: Middleware
    middleware = case env ^. cEnvironment of
      Production -> logStdout . gzip def
      _          -> logStdoutDev

shutdownApp :: Env -> IO ()
shutdownApp = destroyAllResources . view  pool
