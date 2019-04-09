module Api (app) where

import Control.Monad.Logger (runStderrLoggingT)
import Data.Proxy (Proxy)

import Servant
import Servant.Auth.Server

import Agent.Api (AgentApi, agentServer)
import Evaluation.Api (EvaluationApi, evaluationServer)
import Foundation (App, AppT(..), Config, HasConfig(..), filterLogs)
import User.Api (UserApi, userServer)

-- API specification
type Api auths = "api" :>
  (    UserApi auths
    :<|> AgentApi auths
    :<|> EvaluationApi auths
  )

api :: Proxy (Api '[JWT])
api = Proxy

-- composition of various handler servers
appServerT
  :: CookieSettings
  -> JWTSettings
  -> ServerT (Api auths) App
appServerT cs jws  =
       userServer cs jws
  :<|> agentServer
  :<|> evaluationServer

convertAppT :: forall a. Config -> App a -> Handler a
convertAppT conf appM =
    Handler . ExceptT . try . runStderrLoggingT
  $ filterLogs (conf ^. cEnvironment)
  $ usingReaderT conf
  $ runApp appM

proxyContext :: Proxy '[CookieSettings, JWTSettings]
proxyContext = Proxy

appServer :: Config -> CookieSettings -> JWTSettings -> Server (Api '[JWT])
appServer conf cs jws = hoistServerWithContext
  api
  proxyContext
  (convertAppT conf)
  (appServerT cs jws)

app :: Config -> IO Application
app conf = do
  -- we generate the key for signing tokens. This would generally be persisted
  -- should we persist ??
  myKey <- generateKey

  let jwtCfg = defaultJWTSettings myKey
      cfg = defaultCookieSettings :. jwtCfg :. EmptyContext

  pure $ serveWithContext api cfg (appServer conf defaultCookieSettings jwtCfg)
