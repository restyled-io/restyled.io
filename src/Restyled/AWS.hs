module Restyled.AWS
  ( Env
  , HasAWS (..)
  , AWSRequest
  , AWSResponse
  , AWSPager
  , discover
  , send
  , paginate

    -- * Re-export
  , MonadResource
  ) where

import Restyled.Prelude hiding (hoistEither)

import Amazonka (AWSPager, AWSRequest, AWSResponse, Env)
import qualified Amazonka as AWS
import Amazonka.Env (env_logger)
import Conduit
import qualified Control.Monad.Logger as Logger
import Lens.Micro ((.~))
import Yesod.Core.Types (HandlerData)
import Yesod.Core.Types.Lens

class HasAWS env where
  awsEnvL :: Lens' env AWS.Env

instance HasAWS AWS.Env where
  awsEnvL = id

instance HasAWS env => HasAWS (HandlerData child env) where
  awsEnvL = envL . siteL . awsEnvL

discover :: MonadLoggerIO m => m Env
discover = do
  loggerIO <- askLoggerIO

  let logger level msg = do
        loggerIO
          Logger.defaultLoc
          "Amazonka"
          (fromLevel level)
          (Logger.toLogStr msg)

  env <- liftIO $ AWS.newEnv AWS.discover
  pure $ env & env_logger .~ logger

fromLevel :: AWS.LogLevel -> LogLevel
fromLevel = \case
  AWS.Info -> LevelInfo
  AWS.Error -> LevelError
  AWS.Debug -> LevelDebug
  AWS.Trace -> LevelOther "trace"

send
  :: ( MonadResource m
     , MonadReader env m
     , HasAWS env
     , AWSRequest a
     , Typeable a
     , Typeable (AWSResponse a)
     )
  => a
  -> m (AWSResponse a)
send req = do
  env <- view awsEnvL
  AWS.send env req

paginate
  :: ( MonadResource m
     , MonadReader env m
     , HasAWS env
     , AWSPager a
     , Typeable a
     , Typeable (AWSResponse a)
     )
  => a
  -> ConduitM () (AWSResponse a) m ()
paginate req = do
  env <- view awsEnvL
  AWS.paginateEither env req >>= hoistEither

hoistEither :: MonadIO m => Either AWS.Error a -> m a
hoistEither = either (liftIO . throwIO) pure
