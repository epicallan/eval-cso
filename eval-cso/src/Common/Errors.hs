module Common.Errors
       ( throwSError
       , eitherSError
       , MonadThrowLogger
       ) where
import Control.Monad.Logger (MonadLogger, logErrorN)
import Data.Aeson (ToJSON, encode)
import Network.HTTP.Types.Header (Header)
import Servant (ServantErr(..))

type MonadThrowLogger m = (MonadLogger m, MonadThrow m)


data JSONError = JSONError
  { status :: Int
  , message :: Text
  } deriving (Generic, Show)

instance ToJSON JSONError

throwSError
  :: (Exception e, MonadThrowLogger m)
  => ServantErr
  -> e
  -> m a
throwSError sError exception = do
  logErrorN $ show exception
  throwM $ sError {errBody = encode encodeJSONError, errHeaders = jsonHeader}
  where
    jsonHeader :: [Header]
    jsonHeader = [("Content-Type", "application/json")]

    encodeJSONError :: JSONError
    encodeJSONError = JSONError
      { status = errHTTPCode sError
      , message = show exception
      }

eitherSError
  :: (Exception e, MonadThrowLogger m)
  => ServantErr
  -> Either e a
  -> m a
eitherSError sError = either (throwSError sError) pure
