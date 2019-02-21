module Api (app) where

import Data.Proxy (Proxy)
import Servant
import Servant.Server (Application, Handler(..), hoistServer)

import Config (AppT(..), Env, AppEffs)
import User.Api (UserApi, userServer)

-- API specification
type Api = "user" :> UserApi
  -- :<|> "article"  :> ArticleApi

api :: Proxy Api
api = Proxy

-- composition of various handler servers
appServerT :: forall m. AppEffs Env m => ServerT Api m
appServerT = userServer

convertAppT :: Env -> AppT IO a -> Handler a
convertAppT env appM =
  Handler . ExceptT . try . runReaderT (runApp appM) $ env

appServer :: Env -> Server Api
appServer env = hoistServer api (convertAppT env) appServerT

app :: Env -> Application
app env = serve api (appServer env)
