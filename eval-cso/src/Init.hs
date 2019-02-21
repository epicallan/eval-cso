module Init
        ( runApp
        ) where

import Network.Wai.Handler.Warp (run)

-- import qualified Data.Pool                   as Pool

import Api (app)
import Config (Env, HasConfig(..), initEnv)

runApp :: IO ()
runApp = bracket initEnv shutdownApp startApp
    where
        startApp :: HasConfig Env => Env -> IO ()
        startApp env = run (env ^. cPort) (app env)


-- | Takes care of cleaning up 'Config' resources
shutdownApp :: Env -> IO ()
-- shutdownApp config = Pool.destroyAllResources (config ^. cPort)
-- use close' from Database-persist
shutdownApp = error "implement me"

