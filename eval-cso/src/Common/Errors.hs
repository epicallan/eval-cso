module Common.Errors
       ( throwSError
       , eitherSError
       ) where

import Servant (ServantErr(..))

throwSError
  :: (MonadThrow m, Exception e)
  => ServantErr
  -> e
  -> m a
throwSError sError ex = throwM $ sError {errBody = show ex}


eitherSError
  :: (MonadThrow m, Exception e)
  => ServantErr
  -> Either e a
  -> m a
eitherSError sError = either (throwSError sError) pure
