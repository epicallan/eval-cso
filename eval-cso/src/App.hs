module App (runApp) where

import Network.Wai.Handler.Warp (run)

import Api (app)
import Foundation (Env, HasConfig(..), initEnv)

runApp :: IO ()
runApp = bracket initEnv shutdownApp startApp
    where
        startApp :: Env -> IO ()
        startApp env = app env >>= run (env ^. cPort)


-- | Takes care of cleaning up 'Config' resources
shutdownApp :: Env -> IO ()
-- shutdownApp config = Pool.destroyAllResources (config ^. cPort)
-- use close' from Database-persist
shutdownApp = error "implement me"

