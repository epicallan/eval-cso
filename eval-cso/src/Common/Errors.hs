module Common.Errors
       ( throwSError
       , eitherSError
       , MonadThrowLogger
       ) where
import Control.Monad.Logger (MonadLogger, logErrorN)
import Servant (ServantErr(..))

type MonadThrowLogger m = (MonadLogger m, MonadThrow m)

throwSError
  :: (Exception e, MonadThrowLogger m)
  => ServantErr
  -> e
  -> m a
throwSError sError ex = do
  logErrorN $ show ex
  throwM $ sError {errBody = show ex}

eitherSError
  :: (Exception e, MonadThrowLogger m)
  => ServantErr
  -> Either e a
  -> m a
eitherSError sError = either (throwSError sError) pure
