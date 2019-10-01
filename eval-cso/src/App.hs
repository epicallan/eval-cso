{-# LANGUAGE TypeApplications #-}
module App (runApp) where

import Data.Pool (destroyAllResources)
import Network.HTTP.Types (Status(..), hContentType)
import Network.HTTP.Types.Method (methodDelete, methodPatch, methodPut)
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors
  (CorsResourcePolicy(..), cors, simpleCorsResourcePolicy, simpleMethods)
import Network.Wai.Middleware.Gzip (def, gzip)
import Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)
import Network.Wai.Middleware.Servant.Errors
  (ErrorLabels(..), ErrorMsg(..), StatusCode(..), encodeAsJsonError,
  errorMwDefJson)
import System.Remote.Monitoring (forkServer)

import Api (app)
import Foundation
  (Config, Environment(..), HasConfig(..), HasSettings(..), initEnv, pool)

runApp :: IO ()
runApp = bracket initEnv shutdownApp startApp

startApp :: Config -> IO ()
startApp conf = do
  when (conf ^. cEnvironment == Development) (void $ forkServer "127.0.0.1" 8080)
  putTextLn $ "starting " <> conf ^. sAppName <> " on port " <> show (conf ^. sPort)
  app conf >>= runSettings warpSettings . middleware
  where
    port = fromIntegral $ conf ^. sPort

    warpSettings =
        setOnException errorLogger
      $ setOnExceptionResponse response500
      $ setPort port defaultSettings

    response500 :: SomeException -> Response
    response500 exception =
      let errLabels = ErrorLabels {errName = "error", errStatusName = "status" }
          errMsg = ErrorMsg (show exception)
          response = encodeAsJsonError errLabels (StatusCode 500)  errMsg
          header = (hContentType,  "application/json")
          status = Status 500 "Internal Error"
      in responseLBS status [header] response

    errorLogger :: Maybe Request -> SomeException -> IO ()
    errorLogger _ e = putStrLn $ "Internal Error: " <> show @Text e

    middleware :: Middleware
    middleware = appCors . errorMwDefJson . case conf ^. cEnvironment of
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
