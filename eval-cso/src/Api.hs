module Api (app) where

import Control.Monad.Logger (runStderrLoggingT)
import Data.Proxy (Proxy)

import Servant
import Servant.Auth.Server

import Agent.Api (AgentApi, agentServer)
import Evaluation.Api (EvaluationApi, evaluationServer)
import Foundation (App, AppT(..), Env, HasConfig(..), filterLogs)
import User.Api (UserApi, userServer)

-- API specification
type Api auths =
      "api" :> UserApi auths
  :<|> AgentApi auths
  :<|> EvaluationApi auths

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

convertAppT :: forall a. Env -> App a -> Handler a
convertAppT env appM =
    Handler . ExceptT . try . runStderrLoggingT
  $ filterLogs (env ^. cEnvironment)
  $ runReaderT (runApp appM) env

proxyContext :: Proxy '[CookieSettings, JWTSettings]
proxyContext = Proxy

appServer :: Env -> CookieSettings -> JWTSettings -> Server (Api '[JWT])
appServer env cs jws = hoistServerWithContext
  api
  proxyContext
  (convertAppT env)
  (appServerT cs jws)

app :: Env -> IO Application
app env = do
  -- we generate the key for signing tokens. This would generally be persisted
  -- should we persist ??
  myKey <- generateKey

  let jwtCfg = defaultJWTSettings myKey
      cfg = defaultCookieSettings :. jwtCfg :. EmptyContext

  pure $ serveWithContext api cfg (appServer env defaultCookieSettings jwtCfg)
